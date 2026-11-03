using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BinderFactory
{
    private readonly Compilation _compilation;

    public BinderFactory(Compilation compilation)
    {
        _compilation = compilation;
    }

    public Binder? GetBinder(SyntaxNode node, Binder? parentBinder = null)
    {
        // Now safely construct this node's binder with a guaranteed parent
        Binder? newBinder = node switch
        {
            AttributeListSyntax attributeList => CreateAttributeBinder(attributeList, parentBinder!),
            AttributeSyntax attribute when attribute.Parent is AttributeListSyntax attributeList => CreateAttributeBinder(attributeList, parentBinder!),
            BaseNamespaceDeclarationSyntax ns => CreateNamespaceBinder(ns, parentBinder!),
            MethodDeclarationSyntax => parentBinder,
            TypeSyntax when parentBinder is FunctionBinder functionBinder &&
                node.Ancestors().Any(ancestor => ancestor is TypeConstraintSyntax)
                => functionBinder.GetMethodBodyBinder(),
            BlockSyntax => CreateBlockBinder(parentBinder),
            BlockStatementSyntax => CreateBlockBinder(parentBinder),
            ArrowExpressionClauseSyntax => CreateBlockBinder(parentBinder),
            IfExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            IfPatternExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            IfStatementSyntax stmt => new LocalScopeBinder(parentBinder!),
            IfPatternStatementSyntax stmt => new LocalScopeBinder(parentBinder!),
            ElseExpressionClauseSyntax elseClause => new LocalScopeBinder(parentBinder!),
            WhileStatementSyntax stmt => new BlockBinder(parentBinder!.ContainingSymbol, parentBinder!),
            WhilePatternStatementSyntax stmt => new BlockBinder(parentBinder!.ContainingSymbol, parentBinder!),
            LoopStatementSyntax stmt => new BlockBinder(parentBinder!.ContainingSymbol, parentBinder!),
            ForStatementSyntax stmt => new BlockBinder(parentBinder!.ContainingSymbol, parentBinder!),
            FunctionStatementSyntax localFunc => new FunctionBinder(parentBinder!, localFunc),
            MacroDeclarationSyntax macro => new MacroBinder(parentBinder!, macro),
            // Type declaration binders are created and cached by SemanticModel
            ClassDeclarationSyntax => parentBinder,
            RecordDeclarationSyntax => parentBinder,
            StructDeclarationSyntax => parentBinder,
            InterfaceDeclarationSyntax => parentBinder,
            UnionDeclarationSyntax => parentBinder,
            //FieldDeclarationSyntax => parent, // Fields are handled during symbol declaration
            _ => parentBinder
        };

        return newBinder;
    }

    private Binder CreateAttributeBinder(AttributeListSyntax attributeList, Binder parentBinder)
    {
        var owner = ResolveAttributeOwner(attributeList, parentBinder)
            ?? parentBinder.ContainingSymbol
            ?? _compilation.Assembly;

        return new AttributeBinder(owner, parentBinder);
    }

    private Symbol? ResolveAttributeOwner(AttributeListSyntax attributeList, Binder parentBinder)
    {
        var parentNode = attributeList.Parent;
        if (parentNode is null)
            return null;

        if (AttributeUsageHelper.IsNamespaceContainerAttributeDeclaration(attributeList))
        {
            var namespaceName = string.Join(
                ".",
                parentNode.Ancestors()
                    .OfType<BaseNamespaceDeclarationSyntax>()
                    .Reverse()
                    .Select(static current => current.Name.ToValueString()));
            var namespaceSymbol = string.IsNullOrEmpty(namespaceName)
                ? _compilation.SourceGlobalNamespace
                : _compilation.GetOrCreateNamespaceSymbol(namespaceName)?.AsSourceNamespace();
            if (namespaceSymbol is not null)
                return _compilation.GetOrCreateNamespaceMembersContainer(namespaceSymbol, parentNode);
        }

        if (parentNode is CompilationUnitSyntax)
            return _compilation.Assembly;

        if (parentNode is TypeDeclarationSyntax { ParameterList: not null } typeDeclaration &&
            HasExplicitAttributeTarget(attributeList, "method") &&
            TryResolvePrimaryConstructor(typeDeclaration, parentBinder, out var primaryConstructor))
        {
            return primaryConstructor;
        }

        return parentBinder.BindDeclaredSymbol(parentNode);
    }

    private static bool TryResolvePrimaryConstructor(
        TypeDeclarationSyntax declaration,
        Binder parentBinder,
        out MethodSymbol primaryConstructor)
    {
        if (parentBinder.BindDeclaredSymbol(declaration) is NamedTypeSymbol type)
        {
            foreach (var constructor in type.GetMembers().OfType<MethodSymbol>())
            {
                if (constructor.MethodKind != MethodKind.Constructor)
                    continue;

                if (constructor.DeclaringSyntaxReferences.Any(reference => reference.GetSyntax() == declaration))
                {
                    primaryConstructor = constructor;
                    return true;
                }
            }
        }

        primaryConstructor = null!;
        return false;
    }

    private static bool HasExplicitAttributeTarget(AttributeListSyntax attributeList, string targetName)
        => string.Equals(
            attributeList.Target?.Identifier.ValueText,
            targetName,
            StringComparison.OrdinalIgnoreCase);

    private Binder? CreateBlockBinder(Binder? parentBinder)
    {
        if (parentBinder is MacroBinder macroBinder)
            return new MacroBodyBinder(macroBinder.GetMacroSymbol(), parentBinder);

        // A block directly under a method requires a MethodBodyBinder so the method body
        // can bind parameters and local declarations. Nested blocks use a forwarding
        // binder boundary; BlockBinder owns their lexical locals.
        var methodSymbol = GetMethodSymbol(parentBinder);
        return methodSymbol is not null
            ? new MethodBodyBinder(methodSymbol, parentBinder!)
            : new LocalScopeBinder(parentBinder!);
    }

    private static MethodSymbol? GetMethodSymbol(Binder? parentBinder)
    {
        if (parentBinder is MethodBinder methodBinder)
            return methodBinder?.GetMethodSymbol()!;

        if (parentBinder is FunctionBinder functionBinder)
            return functionBinder?.GetMethodSymbol()!;

        if (parentBinder is FunctionExpressionBinder functionExpressionBinder &&
            functionExpressionBinder.ContainingSymbol is MethodSymbol lambdaMethod)
        {
            return lambdaMethod;
        }

        return null;
    }

    private Binder CreateNamespaceBinder(BaseNamespaceDeclarationSyntax nsSyntax, Binder parentBinder)
    {
        var parentNamespace = parentBinder.CurrentNamespace;
        var namespaceName = nsSyntax is FileScopedNamespaceDeclarationSyntax
            ? nsSyntax.Name.ToValueString()
            : parentNamespace.QualifyName(nsSyntax.Name.ToValueString());

        var nsSymbol = _compilation.GetOrCreateNamespaceSymbol(namespaceName)
            ?? throw new InvalidOperationException($"Unable to resolve namespace '{namespaceName}'.");

        var nsBinder = new NamespaceBinder(parentBinder, nsSymbol);

        var namespaceImports = new List<NamespaceOrTypeSymbol>();
        var typeImports = new List<TypeSymbol>();
        var aliases = new Dictionary<string, IReadOnlyList<AliasSymbol>>();

        var provisionalImportBinder = new ImportBinder(nsBinder, namespaceImports, typeImports, aliases);
        var inheritedImportKeys = CollectInheritedImportKeys();

        foreach (var importDirective in nsSyntax.Imports)
        {
            var importName = importDirective.Name.ToValueString();
            if (inheritedImportKeys.Contains(importName))
            {
                nsBinder.Diagnostics.ReportImportDirectiveRedundantWithGlobalImport(
                    importName,
                    importDirective.GetLocation());
                continue;
            }

            if (IsWildcard(importDirective.Name, out var nsName))
            {
                var nsImport = ResolveWildcardImportTarget(nsSymbol!, nsName);
                if (nsImport != null)
                    namespaceImports.Add(nsImport);
                else
                    nsBinder.Diagnostics.ReportInvalidImportTarget(nsName.GetLocation());
                continue;
            }

            if (IsIncompleteImportName(importDirective.Name))
                continue;

            var namespaceSymbol = ResolveNamespace(nsSymbol!, importName);
            if (namespaceSymbol != null)
            {
                nsBinder.Diagnostics.ReportTypeExpectedWithoutWildcard(importDirective.Name.GetLocation());
                continue;
            }

            TypeSymbol? typeSymbol = HasTypeArguments(importDirective.Name)
                ? ResolveOpenGenericType(nsSymbol!, importDirective.Name)
                : ResolveType(nsSymbol!, importName);

            if (typeSymbol != null)
            {
                typeImports.Add(typeSymbol);
            }
            else if (TryResolveImportedConstant(nsSymbol!, importDirective.Name) is { } constant)
            {
                AddAliasImport(constant.Name, constant);
            }
            else if (TryResolveImportedNamespaceMember(nsSymbol!, importDirective.Name) is { } namespaceMember)
            {
                AddAliasImport(namespaceMember.Name, namespaceMember);
            }
            else
            {
                nsBinder.Diagnostics.ReportInvalidImportTarget(importDirective.Name.GetLocation());
            }
        }

        foreach (var aliasDirective in nsSyntax.Aliases)
        {
            IReadOnlyList<Symbol> symbols;
            if (aliasDirective.Target is NameSyntax name)
            {
                symbols = ResolveAlias(nsSymbol!, name);
            }
            else
            {
                var typeSymbol = TryResolveTypeSyntaxSilently(aliasDirective.Target);
                symbols = typeSymbol is null
                    ? Array.Empty<Symbol>()
                    : new Symbol[] { typeSymbol };
            }

            if (symbols.Count > 0)
            {
                var aliasName = aliasDirective.Identifier.ValueText;
                var aliasSymbols = symbols
                    .Select(s => AliasSymbolFactory.Create(aliasName, s))
                    .ToArray();
                aliases[aliasName] = aliasSymbols;
            }
            else
            {
                nsBinder.Diagnostics.ReportInvalidAliasType(aliasDirective.Target.GetLocation());
            }
        }

        var importBinder = new ImportBinder(nsBinder, namespaceImports, typeImports, aliases);

        foreach (var diagnostic in nsBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        return importBinder;

        HashSet<string> CollectInheritedImportKeys()
        {
            var keys = CollectGlobalImportKeys();

            if (nsSyntax.SyntaxTree.GetRoot() is CompilationUnitSyntax compilationUnit)
            {
                foreach (var import in compilationUnit.Imports)
                    keys.Add(import.Name.ToValueString());
            }

            foreach (var ancestorNamespace in nsSyntax
                         .Ancestors()
                         .OfType<BaseNamespaceDeclarationSyntax>())
            {
                foreach (var import in ancestorNamespace.Imports)
                    keys.Add(import.Name.ToValueString());
            }

            return keys;
        }

        HashSet<string> CollectGlobalImportKeys()
        {
            var keys = new HashSet<string>(StringComparer.Ordinal);

            foreach (var tree in _compilation.SyntaxTrees)
            {
                if (tree.GetRoot() is not CompilationUnitSyntax root)
                    continue;

                foreach (var globalImport in root.Members.OfType<GlobalImportBlockSyntax>())
                {
                    foreach (var import in globalImport.Imports)
                        keys.Add(import.Name.ToValueString());
                }
            }

            return keys;
        }

        NamespaceSymbol? ResolveNamespace(NamespaceSymbol current, string name)
        {
            var full = Combine(current, name);
            return _compilation.GetNamespaceSymbol(full) ?? _compilation.GetNamespaceSymbol(name);
        }

        TypeSymbol? ResolveType(NamespaceSymbol current, string name)
        {
            // Metadata found during declaration setup is provisional, even when
            // namespace lookup completion is not explicitly suppressed. Resolve the
            // exact indexed source candidate before retaining a bootstrap alias target.
            return ResolveIndexedSourceType(name)
                ?? ResolveIndexedSourceType(Combine(current, name))
                ?? ResolveMetadataTypeByName(name)
                ?? ResolveScopedMetadataType(current, name)
                ?? ResolveTypeFromContainingNamespace(current, name)
                ?? ResolveTypeFromNamespace(current, name)
                ?? ResolveTypeFromGlobalRoots(name);
        }

        NamedTypeSymbol? ResolveIndexedSourceType(string name)
        {
            var separator = name.LastIndexOf('.');
            var namespaceName = separator < 0 ? string.Empty : name[..separator];
            var typeName = name[(separator + 1)..];
            var scope = string.IsNullOrEmpty(namespaceName)
                ? _compilation.SourceGlobalNamespace
                : _compilation.GetNamespaceSymbol(namespaceName);
            if (scope is null)
                return null;

            return _compilation.TryDeclareIndexedSourceType(scope, typeName, out var sourceType) && sourceType.Arity == 0
                ? sourceType
                : null;
        }

        TypeSymbol? ResolveTypeFromGlobalRoots(string name)
        {
            var sourceType = ResolveTypeFromNamespace(_compilation.SourceGlobalNamespace, name);
            if (sourceType is not null)
                return sourceType;

            foreach (var assembly in _compilation.ReferencedAssemblySymbols)
            {
                var metadataType = ResolveTypeFromNamespace(assembly.GlobalNamespace, name);
                if (metadataType is not null)
                    return metadataType;
            }

            return null;
        }

        NamedTypeSymbol? ResolveMetadataTypeByName(string name)
            => _compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
                ? _compilation.TryGetMetadataReferenceTypeByMetadataName(name)
                : _compilation.GetTypeByMetadataName(name);

        NamedTypeSymbol? ResolveScopedMetadataType(NamespaceSymbol current, string name)
            => _compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
                ? _compilation.TryGetMetadataReferenceTypeByMetadataName(current, name)
                : _compilation.GetTypeByMetadataName(current, name);

        TypeSymbol? ResolveTypeFromContainingNamespace(NamespaceSymbol current, string name)
        {
            var lastDot = name.LastIndexOf('.');
            if (lastDot <= 0 || lastDot == name.Length - 1)
                return null;

            var namespaceName = name[..lastDot];
            var typeName = name[(lastDot + 1)..];
            var ns = ResolveNamespace(current, namespaceName);
            return (!_compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed ? ns?.LookupType(typeName) : null)
                ?? ns?.GetTypeMembers(typeName, arity: 0).FirstOrDefault();
        }

        TypeSymbol? ResolveTypeFromNamespace(NamespaceSymbol scope, string name)
        {
            if (string.IsNullOrEmpty(name))
                return null;

            var parts = name.Split('.');
            NamespaceOrTypeSymbol current = scope;

            foreach (var part in parts)
            {
                if (current is NamespaceSymbol ns)
                {
                    var typeMember = (!_compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed ? ns.LookupType(part) : null)
                        ?? ns.GetTypeMembers(part, arity: 0).FirstOrDefault();
                    if (typeMember is not null)
                    {
                        current = typeMember;
                        continue;
                    }

                    var namespaceMember = ns.LookupNamespace(part)
                        ?? ns.GetMembers(part).OfType<NamespaceSymbol>().FirstOrDefault();
                    if (namespaceMember is null)
                        return null;

                    current = namespaceMember;
                    continue;
                }

                if (current is NamedTypeSymbol type)
                {
                    var nestedType = type.GetTypeMembers(part, arity: 0).FirstOrDefault();
                    if (nestedType is null)
                        return null;

                    current = nestedType;
                    continue;
                }

                return null;
            }

            return current as TypeSymbol;
        }

        IReadOnlyList<Symbol> ResolveAlias(NamespaceSymbol current, NameSyntax name)
        {
            var nsSymbol = ResolveNamespace(current, name.ToValueString());
            if (nsSymbol != null)
                return [nsSymbol];

            TypeSymbol? typeSymbol = HasTypeArguments(name)
                ? ResolveGenericType(current, name)
                : ResolveType(current, name.ToValueString());
            if (typeSymbol != null)
                return [typeSymbol];

            if (name is QualifiedNameSyntax qn)
            {
                var memberName = GetRightmostIdentifier(name);
                var left = qn.Left;
                var leftHasTypeArguments = HasTypeArguments(left);

                TypeSymbol? containingType = leftHasTypeArguments
                    ? ResolveGenericType(current, left) ?? TryResolveTypeSyntaxSilently(left)
                    : ResolveType(current, left.ToValueString());

                if (containingType != null)
                {
                    if (!leftHasTypeArguments &&
                        containingType is UnionSymbol unionSymbol)
                    {
                        var variants = unionSymbol.Variants
                            .Where(type => string.Equals(type.Name, memberName, StringComparison.Ordinal))
                            .Cast<Symbol>()
                            .ToArray();

                        if (variants.Length > 0)
                            return variants;
                    }

                    var members = containingType.GetMembers(memberName)
                        .Where(m => m.IsStatic)
                        .ToArray();

                    if (qn.Right is GenericNameSyntax genericRight &&
                        TryResolveTypeArgumentsSilently(genericRight.TypeArgumentList, out var memberTypeArguments))
                    {
                        var closedTypeMembers = members
                            .OfType<NamedTypeSymbol>()
                            .Where(t => t.Arity == memberTypeArguments.Length)
                            .Select(t => t.Construct(memberTypeArguments))
                            .Cast<Symbol>()
                            .ToArray();

                        if (closedTypeMembers.Length > 0)
                            return closedTypeMembers;
                    }

                    if (qn.Right is IdentifierNameSyntax)
                    {
                        members = members
                            .Where(static member => member is not NamedTypeSymbol type || type.Arity == 0)
                            .ToArray();
                    }

                    if (members.Length > 0)
                        return members;
                }

                if (TryResolveImportedNamespaceMember(current, name) is { } namespaceMember)
                    return [namespaceMember];
            }

            return Array.Empty<Symbol>();
        }

        static bool HasTypeArguments(NameSyntax nameSyntax)
            => nameSyntax.DescendantNodes().OfType<GenericNameSyntax>().Any();

        static bool IsIncompleteImportName(NameSyntax nameSyntax)
            => nameSyntax.DescendantNodesAndSelf()
                .OfType<SimpleNameSyntax>()
                .Any(static name => name.Identifier.IsMissing);

        TypeSymbol? ResolveGenericType(NamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = g.Identifier.ValueText + "`" + g.TypeArgumentList.Arguments.Count;
                var unconstructed = ResolveScopedMetadataType(current, baseName);
                if (unconstructed is null)
                    return null;

                if (!TryResolveTypeArgumentsSilently(g.TypeArgumentList, out var args))
                    return null;

                return _compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = leftName + "." + gen.Identifier.ValueText + "`" + gen.TypeArgumentList.Arguments.Count;
                var unconstructed = ResolveMetadataTypeByName(baseName)
                    ?? ResolveScopedMetadataType(current, baseName);
                if (unconstructed is null)
                    return null;

                if (!TryResolveTypeArgumentsSilently(gen.TypeArgumentList, out var args))
                    return null;

                return _compilation.ConstructGenericType(unconstructed, args);
            }

            return null;
        }

        TypeSymbol? ResolveOpenGenericType(NamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = g.Identifier.ValueText + "`" + (g.TypeArgumentList.Arguments.SeparatorCount + 1);
                var unconstructed = ResolveScopedMetadataType(current, baseName);
                if (unconstructed is null)
                    return null;

                if (!TryResolveTypeArgumentsSilently(g.TypeArgumentList, out var args))
                    return null;

                return _compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = leftName + "." + gen.Identifier.ValueText + "`" + (gen.TypeArgumentList.Arguments.SeparatorCount + 1);
                var unconstructed = ResolveMetadataTypeByName(baseName)
                    ?? ResolveScopedMetadataType(current, baseName);
                if (unconstructed is not null)
                    return unconstructed;
            }

            return null;
        }

        TypeSymbol? TryResolveTypeSyntaxSilently(TypeSyntax syntax)
        {
            using (provisionalImportBinder.Diagnostics.CreateNonReportingScope())
            {
                var result = provisionalImportBinder.BindTypeSyntax(syntax);
                return result.Success ? result.ResolvedType : null;
            }
        }

        NamespaceOrTypeSymbol? ResolveWildcardImportTarget(NamespaceSymbol current, NameSyntax name)
        {
            var targetName = GetRightmostIdentifier(name);
            var nameText = name.ToValueString();

            if (ResolveType(current, nameText) is { } resolvedType &&
                string.Equals(resolvedType.Name, targetName, StringComparison.Ordinal))
                return GetLogicalImportType(resolvedType);

            var resolvedNamespace = ResolveNamespace(current, nameText);
            if (resolvedNamespace is not null &&
                provisionalImportBinder.TryResolveTypeFromNamespaceName(resolvedNamespace, out var namespaceType))
            {
                return namespaceType;
            }

            return resolvedNamespace;
        }

        static TypeSymbol GetLogicalImportType(TypeSymbol type)
        {
            if (type is UnionCompanionSymbol companion &&
                companion.TryGetAssociatedUnion(out var union))
            {
                return (TypeSymbol)union;
            }

            return type;
        }

        FieldSymbol? TryResolveImportedConstant(NamespaceSymbol current, NameSyntax name)
        {
            if (name is not QualifiedNameSyntax { Left: var left, Right: IdentifierNameSyntax right })
                return null;

            var ownerType = ResolveType(current, left.ToValueString())
                ?? TryResolveTypeSyntaxSilently(left);
            if (ownerType is null)
                return null;

            return ownerType.GetMembers(right.Identifier.ValueText)
                .OfType<FieldSymbol>()
                .FirstOrDefault(IsImportableConstant);
        }

        Symbol? TryResolveImportedNamespaceMember(NamespaceSymbol current, NameSyntax name)
        {
            if (name is not QualifiedNameSyntax { Left: var left, Right: IdentifierNameSyntax right })
                return null;

            var ns = ResolveNamespace(current, left.ToValueString());
            if (ns is null)
                return null;

            return _compilation.GetNamespaceMembers(ns, right.Identifier.ValueText, _compilation.Options.AllowNamespaceMemberImports)
                .FirstOrDefault(static member => member is MethodSymbol or FieldSymbol);
        }

        void AddAliasImport(string name, Symbol symbol)
        {
            var alias = AliasSymbolFactory.Create(name, symbol);
            if (aliases.TryGetValue(name, out var existing))
            {
                aliases[name] = existing.Concat([alias]).ToArray();
                return;
            }

            aliases[name] = [alias];
        }

        bool TryResolveTypeArgumentsSilently(TypeArgumentListSyntax list, out TypeSymbol[] args)
        {
            args = new TypeSymbol[list.Arguments.Count];

            for (var i = 0; i < list.Arguments.Count; i++)
            {
                var resolved = TryResolveTypeSyntaxSilently(list.Arguments[i].Type);
                if (resolved is null)
                    return false;

                args[i] = resolved;
            }

            return true;
        }

        static string Combine(NamespaceSymbol current, string name)
        {
            var currentName = current.ToString();
            return string.IsNullOrEmpty(currentName) ? name : currentName + "." + name;
        }

        static string GetRightmostIdentifier(NameSyntax nameSyntax)
        {
            return nameSyntax switch
            {
                IdentifierNameSyntax id => id.Identifier.ValueText,
                QualifiedNameSyntax qn => GetRightmostIdentifier(qn.Right),
                _ => nameSyntax.ToString()
            };
        }

        static bool IsImportableConstant(FieldSymbol field)
            => field.IsConst || field.ContainingType?.TypeKind == TypeKind.Enum;

        static bool IsWildcard(NameSyntax nameSyntax, out NameSyntax baseName)
        {
            if (nameSyntax is QualifiedNameSyntax { Right: WildcardNameSyntax, Left: var left })
            {
                baseName = left;
                return true;
            }

            baseName = default!;
            return false;
        }
    }
}
