using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly Dictionary<SyntaxNode, Binder> _binderCache = new();
    private readonly Dictionary<SyntaxNode, SymbolInfo> _symbolMappings = new();
    private readonly Dictionary<SyntaxNode, BoundNode> _boundNodeCache = new();
    private readonly Dictionary<LabeledStatementSyntax, ILabelSymbol> _labelDeclarations = new();
    private readonly Dictionary<ILabelSymbol, LabeledStatementSyntax> _labelSyntax = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<string, List<ILabelSymbol>> _labelsByName = new(StringComparer.Ordinal);
    private readonly Dictionary<GotoStatementSyntax, ILabelSymbol> _gotoTargets = new();
    private readonly Dictionary<AttributeSyntax, AttributeData?> _attributeCache = new();
    private IImmutableList<Diagnostic>? _diagnostics;

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;

        var root = SyntaxTree.GetRoot();
        _ = GetBinder(root);
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    public IImmutableList<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        if (_diagnostics is null)
        {
            EnsureDiagnosticsCollected();

            _diagnostics = _binderCache.Values
                .SelectMany(b => b.Diagnostics.AsEnumerable())
                .Distinct()
                .ToImmutableArray();
        }

        return _diagnostics;
    }

    private void EnsureDiagnosticsCollected()
    {
        var root = SyntaxTree.GetRoot();
        var binder = GetBinder(root);

        Traverse(root, binder);

        void Traverse(SyntaxNode node, Binder currentBinder)
        {
            foreach (var child in node.ChildNodes())
            {
                var childBinder = GetBinder(child, currentBinder);

                if (child is GlobalStatementSyntax global)
                {
                    // Bind the contained statement so locals are registered
                    childBinder.GetOrBind(global.Statement);
                    continue;
                }

                if (child is ExpressionSyntax || child is StatementSyntax)
                {
                    childBinder.GetOrBind(child);
                    continue;
                }

                Traverse(child, childBinder);
            }
        }
    }

    /// <summary>
    /// Gets symbol information about a syntax node
    /// </summary>
    /// <param name="node">The syntax node</param>
    /// <param name="cancellationToken"></param>
    /// <returns>The symbol info</returns>
    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (_symbolMappings.TryGetValue(node, out var symbolInfo))
            return symbolInfo;

        SymbolInfo info;

        if (node is ExpressionSyntax expression)
        {
            EnsureDiagnosticsCollected();
            var boundExpression = GetBoundNode(expression);
            info = boundExpression.GetSymbolInfo();
        }
        else if (node is StatementSyntax statement)
        {
            EnsureDiagnosticsCollected();
            var boundStatement = (BoundStatement)GetBoundNode(statement);
            info = boundStatement.GetSymbolInfo();
        }
        else
        {
            var binder = GetBinder(node);
            info = binder.BindSymbol(node);
        }

        _symbolMappings[node] = info;
        return info;
    }

    /// <summary>
    /// Given a syntax node that declares a method, property, or member accessor, get the corresponding symbol.
    /// </summary>
    /// <param name="node"></param>
    /// <returns></returns>
    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        var binder = GetBinder(node);

        if (node is UnionCaseClauseSyntax caseClause && _unionCaseSymbols.TryGetValue(caseClause, out var caseSymbol))
            return caseSymbol;

        if (Compilation.DeclarationTable.TryGetDeclKey(node, out var key))
        {
            return Compilation.SymbolFactory.GetOrCreate(key, () =>
            {
                return (Symbol)binder.BindDeclaredSymbol(node)!;
            });
        }

        return binder.BindDeclaredSymbol(node);
    }

    /// <summary>
    /// Gets type information about an expression.
    /// </summary>
    /// <param name="expr">The expression syntax node</param>
    /// <returns>The type info</returns>
    public TypeInfo GetTypeInfo(ExpressionSyntax expr)
    {
        // Ensure the tree is fully bound so local declarations preceding the
        // expression are available for lookup. Without this, requesting type
        // information for expressions that reference earlier locals may return
        // null because those locals haven't been bound yet.
        EnsureDiagnosticsCollected();

        var binder = GetBinder(expr);

        var boundExpr = binder.BindExpression(expr);

        if (boundExpr is null)
            return new TypeInfo(null, null);

        return new TypeInfo(boundExpr.Type, boundExpr.GetConvertedType());
    }

    /// <summary>
    /// Gets type information about a type syntax.
    /// </summary>
    /// <param name="typeSyntax">The type syntax node.</param>
    public TypeInfo GetTypeInfo(TypeSyntax typeSyntax)
    {
        var binder = GetBinder(typeSyntax);
        try
        {
            var type = binder.ResolveType(typeSyntax);
            return new TypeInfo(type, type);
        }
        catch
        {
            return new TypeInfo(null, null);
        }
    }

    /// <summary>
    /// Get the bound node for a specific syntax node.
    /// </summary>
    /// <param name="node">The syntax node</param>
    /// <returns>The bound node</returns>
    internal BoundNode GetBoundNode(SyntaxNode node)
    {
        var binder = GetBinder(node);
        return binder.GetOrBind(node);
    }

    internal AttributeData? BindAttribute(AttributeSyntax attribute)
    {
        if (attribute is null)
            throw new ArgumentNullException(nameof(attribute));

        if (_attributeCache.TryGetValue(attribute, out var cached))
            return cached;

        EnsureDiagnosticsCollected();

        BoundExpression? boundExpression = TryGetCachedBoundNode(attribute) as BoundExpression;
        var binder = GetBinder(attribute);

        if (boundExpression is null && binder is AttributeBinder attributeBinder)
            boundExpression = attributeBinder.BindAttribute(attribute);

        var data = AttributeDataFactory.Create(boundExpression, attribute);

        _attributeCache[attribute] = data;
        return data;
    }

    internal void RegisterLabel(LabeledStatementSyntax syntax, ILabelSymbol symbol)
    {
        _labelDeclarations[syntax] = symbol;
        _labelSyntax[symbol] = syntax;

        if (!_labelsByName.TryGetValue(symbol.Name, out var list))
        {
            list = new List<ILabelSymbol>();
            _labelsByName[symbol.Name] = list;
        }

        if (!list.Any(existing => SymbolEqualityComparer.Default.Equals(existing, symbol)))
            list.Add(symbol);
    }

    internal void RegisterGoto(GotoStatementSyntax syntax, ILabelSymbol target)
    {
        _gotoTargets[syntax] = target;
    }

    /// <summary>
    /// Get the bound expression for a specific expression syntax node.
    /// </summary>
    /// <param name="expression">The expression syntax node</param>
    /// <returns>The bound expression</returns>
    /// <remarks>Convenience overload</remarks>
    internal BoundExpression GetBoundNode(ExpressionSyntax expression)
    {
        return (BoundExpression)GetBoundNode((SyntaxNode)expression);
    }

    /// <summary>
    /// Resolves the binder for a specific syntax node.
    /// </summary>
    /// <param name="node">The syntax node</param>
    /// <param name="parentBinder">Be careful</param>
    /// <returns>The binder for the specified syntax node</returns>
    /// <remarks>Might return a cached binder</remarks>
    internal Binder GetBinder(SyntaxNode node, Binder? parentBinder = null)
    {
        if (_binderCache.TryGetValue(node, out var existingBinder))
            return existingBinder;

        // special case for CompilationUnitSyntax
        if (node is CompilationUnitSyntax cu)
        {
            var binder = BindCompilationUnit(cu, parentBinder ?? Compilation.GlobalBinder);
            _binderCache[cu] = binder;
            return binder;
        }

        // Ensure parent binder is constructed and cached first
        Binder? actualParentBinder = parentBinder;

        if (actualParentBinder == null)
        {
            if (!_binderCache.TryGetValue(node.Parent, out actualParentBinder))
            {
                // Recursively create and cache the parent binder first
                actualParentBinder = GetBinder(node.Parent);
            }
        }

        var newBinder = Compilation.BinderFactory.GetBinder(node, actualParentBinder);

        _binderCache[node] = newBinder;
        return newBinder;
    }

    private Binder BindCompilationUnit(CompilationUnitSyntax cu, Binder parentBinder)
    {
        // Step 1: Resolve namespace
        var fileScopedNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();
        INamespaceSymbol targetNamespace;
        NamespaceBinder namespaceBinder;

        if (fileScopedNamespace != null)
        {
            targetNamespace = Compilation.GetOrCreateNamespaceSymbol(fileScopedNamespace.Name.ToString())
                             ?? throw new Exception("Namespace not found");

            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace);
            parentBinder = namespaceBinder;
        }
        else
        {
            targetNamespace = Compilation.SourceGlobalNamespace;
            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace);
            parentBinder = namespaceBinder;
        }

        // Step 2: Handle imports
        var namespaceImports = new List<INamespaceOrTypeSymbol>();
        var typeImports = new List<ITypeSymbol>();
        var aliases = new Dictionary<string, IReadOnlyList<IAliasSymbol>>();
        var deferredWildcardImports = new List<NameSyntax>();

        var provisionalImportBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);

        foreach (var import in cu.DescendantNodes().OfType<ImportDirectiveSyntax>())
        {
            var name = import.Name.ToString();

            if (IsWildcard(import.Name, out var nsName))
            {
                INamespaceOrTypeSymbol? nsImport =
                    (INamespaceOrTypeSymbol?)ResolveNamespace(targetNamespace, nsName.ToString())
                    ?? ResolveType(targetNamespace, nsName.ToString());
                if (nsImport != null)
                {
                    namespaceImports.Add(nsImport);
                }
                else
                {
                    deferredWildcardImports.Add(nsName);
                }

                continue;
            }

            var nsSymbol = ResolveNamespace(targetNamespace, name);
            if (nsSymbol != null)
            {
                namespaceBinder.Diagnostics.ReportTypeExpectedWithoutWildcard(import.Name.GetLocation());
                continue;
            }

            ITypeSymbol? typeSymbol = HasTypeArguments(import.Name)
                ? ResolveOpenGenericType(targetNamespace, import.Name)
                : ResolveType(targetNamespace, name);

            if (typeSymbol != null)
            {
                typeImports.Add(typeSymbol);
            }
            else
            {
                namespaceBinder.Diagnostics.ReportInvalidImportTarget(import.Name.GetLocation());
            }
        }

        var importBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);

        parentBinder = importBinder;

        var compilationUnitBinder = new CompilationUnitBinder(parentBinder, this);
        RegisterNamespaceMembers(cu, compilationUnitBinder, targetNamespace);

        foreach (var baseName in deferredWildcardImports)
        {
            INamespaceOrTypeSymbol? resolved =
                (INamespaceOrTypeSymbol?)ResolveNamespace(targetNamespace, baseName.ToString())
                ?? ResolveType(targetNamespace, baseName.ToString());

            if (resolved != null)
            {
                namespaceImports.Add(resolved);
            }
            else
            {
                namespaceBinder.Diagnostics.ReportInvalidImportTarget(baseName.GetLocation());
            }
        }

        BindAliases(cu.Aliases);

        if (fileScopedNamespace is not null)
        {
            BindAliases(fileScopedNamespace.Aliases);
        }

        foreach (var diagnostic in namespaceBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        foreach (var diagnostic in compilationUnitBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        var topLevelBinder = CreateTopLevelBinder(cu, targetNamespace, importBinder);

        foreach (var diagnostic in importBinder.Diagnostics.AsEnumerable())
            topLevelBinder.Diagnostics.Report(diagnostic);

        _binderCache[cu] = topLevelBinder;
        if (fileScopedNamespace != null)
            _binderCache[fileScopedNamespace] = importBinder;

        return topLevelBinder;

        void BindAliases(SyntaxList<AliasDirectiveSyntax> aliasList)
        {
            foreach (var alias in aliasList)
            {
                IReadOnlyList<ISymbol> symbols;
                if (alias.Target is NameSyntax name)
                {
                    symbols = ResolveAlias(targetNamespace, name);
                }
                else
                {
                    var typeSymbol = provisionalImportBinder.ResolveType(alias.Target);
                    symbols = typeSymbol == Compilation.ErrorTypeSymbol
                        ? Array.Empty<ISymbol>()
                        : new ISymbol[] { typeSymbol };
                }

                if (symbols.Count > 0)
                {
                    var aliasName = alias.Identifier.ValueText;
                    var aliasSymbols = symbols
                        .Select(s => AliasSymbolFactory.Create(aliasName, s))
                        .ToArray();
                    aliases[aliasName] = aliasSymbols;
                }
                else
                {
                    namespaceBinder.Diagnostics.ReportInvalidAliasType(alias.Target.GetLocation());
                }
            }
        }

        INamespaceSymbol? ResolveNamespace(INamespaceSymbol current, string name)
        {
            var full = Combine(current, name);
            return Compilation.GetNamespaceSymbol(full) ?? Compilation.GetNamespaceSymbol(name);
        }

        ITypeSymbol? ResolveType(INamespaceSymbol current, string name)
        {
            var full = Combine(current, name);
            return Compilation.GetTypeByMetadataName(full) ?? Compilation.GetTypeByMetadataName(name);
        }

        IReadOnlyList<ISymbol> ResolveAlias(INamespaceSymbol current, NameSyntax name)
        {
            var nsSymbol = ResolveNamespace(current, name.ToString());
            if (nsSymbol != null)
                return [nsSymbol];

            ITypeSymbol? typeSymbol = HasTypeArguments(name)
                ? ResolveGenericType(current, name)
                : ResolveType(current, name.ToString());
            if (typeSymbol != null)
                return [typeSymbol];

            if (name is QualifiedNameSyntax qn)
            {
                var memberName = GetRightmostIdentifier(name);
                var left = qn.Left;

                ITypeSymbol? containingType = HasTypeArguments(left)
                    ? ResolveGenericType(current, left)
                    : ResolveType(current, left.ToString());

                if (containingType != null)
                {
                    var members = containingType.GetMembers(memberName)
                        .Where(m => m.IsStatic)
                        .ToArray();
                    if (members.Length > 0)
                        return members;
                }
            }

            return Array.Empty<ISymbol>();
        }

        static bool HasTypeArguments(NameSyntax nameSyntax)
            => nameSyntax.DescendantNodes().OfType<GenericNameSyntax>().Any();

        ITypeSymbol? ResolveGenericType(INamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = $"{g.Identifier.ValueText}`{g.TypeArgumentList.Arguments.Count}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = g.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return Compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = $"{leftName}.{gen.Identifier.ValueText}`{gen.TypeArgumentList.Arguments.Count}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = gen.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return Compilation.ConstructGenericType(unconstructed, args);
            }

            return null;
        }

        ITypeSymbol? ResolveOpenGenericType(INamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = $"{g.Identifier.ValueText}`{g.TypeArgumentList.Arguments.SeparatorCount + 1}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = g.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return Compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = $"{leftName}.{gen.Identifier.ValueText}`{gen.TypeArgumentList.Arguments.SeparatorCount + 1}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)Compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is not null)
                    return unconstructed;
            }

            return null;
        }

        static string Combine(INamespaceSymbol current, string name)
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

    private Binder CreateTopLevelBinder(CompilationUnitSyntax cu, INamespaceSymbol namespaceSymbol, Binder parentBinder)
    {
        var fileScopedNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();

        var bindableGlobals = Compilation.CollectBindableGlobalStatements(cu);
        var hasNonGlobalMembers = Compilation.HasNonGlobalMembers(cu);

        var shouldCreateTopLevelProgram = bindableGlobals.Count > 0
            || (!hasNonGlobalMembers && Compilation.Options.OutputKind == OutputKind.ConsoleApplication);

        void CheckOrder(SyntaxList<MemberDeclarationSyntax> members)
        {
            var seenNonGlobal = false;
            foreach (var member in members)
            {
                if (member is ExtensionDeclarationSyntax)
                    continue;

                if (member is GlobalStatementSyntax gs)
                {
                    if (seenNonGlobal)
                        parentBinder.Diagnostics.ReportFileScopedCodeOutOfOrder(gs.GetLocation());
                }
                else
                {
                    seenNonGlobal = true;
                }
            }
        }

        if (fileScopedNamespace != null)
        {
            foreach (var member in cu.Members)
            {
                if (member == fileScopedNamespace)
                    break;

                parentBinder.Diagnostics.ReportFileScopedNamespaceOutOfOrder(member.GetLocation());
            }

            CheckOrder(fileScopedNamespace.Members);
        }
        else
        {
            CheckOrder(cu.Members);
        }

        if (bindableGlobals.Count > 0)
        {
            if (Compilation.Options.OutputKind != OutputKind.ConsoleApplication)
                parentBinder.Diagnostics.ReportFileScopedCodeRequiresConsole(bindableGlobals[0].GetLocation());

            if (Compilation.SyntaxTreeWithFileScopedCode is null)
                Compilation.SyntaxTreeWithFileScopedCode = cu.SyntaxTree;
            else if (Compilation.SyntaxTreeWithFileScopedCode != cu.SyntaxTree)
                parentBinder.Diagnostics.ReportFileScopedCodeMultipleFiles(bindableGlobals[0].GetLocation());
        }

        if (!shouldCreateTopLevelProgram)
            return parentBinder;

        var (programClass, mainMethod, asyncImplementation) = Compilation.GetOrCreateTopLevelProgram(
            cu,
            namespaceSymbol.AsSourceNamespace()!,
            bindableGlobals);

        var scriptMethod = asyncImplementation is { } asyncMain
            ? (SourceMethodSymbol)asyncMain
            : mainMethod;

        var topLevelBinder = new TopLevelBinder(parentBinder, this, scriptMethod, mainMethod, cu);

        // Cache the compilation unit and its global statements before binding.
        //
        // Binding the statements will request declared symbols, which rely on
        // binder lookup through SemanticModel.GetBinder. Without priming the
        // cache, any attempt to resolve the compilation unit binder re-enters
        // BindCompilationUnit, causing unbounded recursion (see stack trace in
        // bug report). By caching eagerly we guarantee re-entrant lookups
        // retrieve the partially constructed top-level binder instead of
        // rebuilding it.
        _binderCache[cu] = topLevelBinder;

        foreach (var stmt in bindableGlobals)
            _binderCache[stmt] = topLevelBinder;

        topLevelBinder.BindGlobalStatements(bindableGlobals);

        return topLevelBinder;

    }

    private void RegisterNamespaceMembers(SyntaxNode containerNode, Binder parentBinder, INamespaceSymbol parentNamespace)
    {
        var classBinders = new List<(ClassDeclarationSyntax Syntax, ClassDeclarationBinder Binder)>();
        var interfaceBinders = new List<(InterfaceDeclarationSyntax Syntax, InterfaceDeclarationBinder Binder)>();
        var extensionBinders = new List<(ExtensionDeclarationSyntax Syntax, ExtensionDeclarationBinder Binder)>();
        var unionBinders = new List<(UnionDeclarationSyntax Syntax, UnionDeclarationBinder Binder, SourceDiscriminatedUnionSymbol Symbol)>();

        foreach (var member in containerNode.ChildNodes())
        {
            switch (member)
            {
                case BaseNamespaceDeclarationSyntax nsDecl:
                    {
                        var namespaceName = nsDecl is FileScopedNamespaceDeclarationSyntax
                            ? nsDecl.Name.ToString()
                            : parentNamespace.QualifyName(nsDecl.Name.ToString());

                        var nsSymbol = Compilation.GetOrCreateNamespaceSymbol(namespaceName);

                        var nsBinder = Compilation.BinderFactory.GetBinder(nsDecl, parentBinder)!;
                        _binderCache[nsDecl] = nsBinder;

                        RegisterNamespaceMembers(nsDecl, nsBinder, nsSymbol);
                        break;
                    }

                case ClassDeclarationSyntax classDecl:
                    {
                        var baseTypeSymbol = Compilation.GetTypeByMetadataName("System.Object");
                        ImmutableArray<INamedTypeSymbol> interfaceList = ImmutableArray<INamedTypeSymbol>.Empty;

                        if (classDecl.BaseList is not null)
                        {
                            var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                            foreach (var t in classDecl.BaseList.Types)
                            {
                                var resolved = parentBinder.ResolveType(t) as INamedTypeSymbol;
                                if (resolved is null)
                                    continue;

                                if (resolved.TypeKind == TypeKind.Interface)
                                    builder.Add(resolved);
                                else
                                    baseTypeSymbol = resolved;
                            }

                            if (builder.Count > 0)
                                interfaceList = builder.ToImmutable();
                        }

                        var isAbstract = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
                        var isSealed = !classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !isAbstract;
                        var isPartial = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.PartialKeyword);
                        var typeAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            classDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));

                        var declarationLocation = classDecl.GetLocation();
                        var declarationReference = classDecl.GetReference();

                        var parentSourceNamespace = parentNamespace.AsSourceNamespace();
                        SourceNamedTypeSymbol classSymbol;
                        var isNewSymbol = true;

                        ReportExternalTypeRedeclaration(
                            parentNamespace,
                            classDecl.Identifier,
                            classDecl.TypeParameterList?.Parameters.Count ?? 0,
                            parentBinder.Diagnostics);

                        if (parentSourceNamespace is not null &&
                            parentSourceNamespace.IsMemberDefined(classDecl.Identifier.ValueText, out var existingMember) &&
                            existingMember is SourceNamedTypeSymbol existingType &&
                            existingType.TypeKind == TypeKind.Class)
                        {
                            var hadPartial = existingType.HasPartialModifier;
                            var hadNonPartial = existingType.HasNonPartialDeclaration;
                            var previouslyMixed = hadPartial && hadNonPartial;
                            var willBeMixed = (hadPartial || isPartial) && (hadNonPartial || !isPartial);

                            if (willBeMixed && !previouslyMixed)
                            {
                                parentBinder.Diagnostics.ReportPartialTypeDeclarationMissingPartial(
                                    classDecl.Identifier.ValueText,
                                    classDecl.Identifier.GetLocation());
                            }
                            else if (hadNonPartial && !isPartial)
                            {
                                parentBinder.Diagnostics.ReportTypeAlreadyDefined(
                                    classDecl.Identifier.ValueText,
                                    classDecl.Identifier.GetLocation());
                            }

                            existingType.AddDeclaration(declarationLocation, declarationReference);
                            existingType.UpdateDeclarationModifiers(isSealed, isAbstract);
                            existingType.RegisterPartialModifier(isPartial);

                            classSymbol = existingType;
                            isNewSymbol = false;
                        }
                        else
                        {
                            classSymbol = new SourceNamedTypeSymbol(
                                classDecl.Identifier.ValueText,
                                baseTypeSymbol!,
                                TypeKind.Class,
                                parentNamespace.AsSourceNamespace(),
                                null,
                                parentNamespace.AsSourceNamespace(),
                                new[] { declarationLocation },
                                new[] { declarationReference },
                                isSealed,
                                isAbstract,
                                declaredAccessibility: typeAccessibility);

                            classSymbol.RegisterPartialModifier(isPartial);
                        }

                        if (isNewSymbol)
                            InitializeTypeParameters(classSymbol, classDecl.TypeParameterList);

                        if (!interfaceList.IsDefaultOrEmpty)
                            classSymbol.SetInterfaces(interfaceList);

                        var classBinder = new ClassDeclarationBinder(parentBinder, classSymbol, classDecl);
                        classBinder.EnsureTypeParameterConstraintTypesResolved(classSymbol.TypeParameters);
                        _binderCache[classDecl] = classBinder;
                        RegisterClassSymbol(classDecl, classSymbol);
                        if (classDecl.BaseList is not null && baseTypeSymbol!.IsSealed)
                            classBinder.Diagnostics.ReportCannotInheritFromSealedType(baseTypeSymbol.Name, classDecl.BaseList.Types[0].GetLocation());

                        classBinders.Add((classDecl, classBinder));
                        break;
                    }

                case UnionDeclarationSyntax unionDecl:
                    {
                        var declaringSymbol = (ISymbol)(parentNamespace.AsSourceNamespace() ?? parentNamespace);
                        var namespaceSymbol = parentNamespace.AsSourceNamespace();
                        ReportExternalTypeRedeclaration(
                            parentNamespace,
                            unionDecl.Identifier,
                            unionDecl.TypeParameterList?.Parameters.Count ?? 0,
                            parentBinder.Diagnostics);
                        var (unionBinder, unionSymbol) = RegisterUnionDeclaration(unionDecl, parentBinder, declaringSymbol, namespaceSymbol);
                        unionBinders.Add((unionDecl, unionBinder, unionSymbol));
                        break;
                    }

                case InterfaceDeclarationSyntax interfaceDecl:
                    {
                        ImmutableArray<INamedTypeSymbol> interfaceList = ImmutableArray<INamedTypeSymbol>.Empty;

                        if (interfaceDecl.BaseList is not null)
                        {
                            var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                            foreach (var t in interfaceDecl.BaseList.Types)
                            {
                                if (parentBinder.ResolveType(t) is INamedTypeSymbol resolved && resolved.TypeKind == TypeKind.Interface)
                                    builder.Add(resolved);
                            }

                            if (builder.Count > 0)
                                interfaceList = builder.ToImmutable();
                        }

                        ReportExternalTypeRedeclaration(
                            parentNamespace,
                            interfaceDecl.Identifier,
                            interfaceDecl.TypeParameterList?.Parameters.Count ?? 0,
                            parentBinder.Diagnostics);

                        var interfaceSymbol = new SourceNamedTypeSymbol(
                            interfaceDecl.Identifier.ValueText,
                            Compilation.GetTypeByMetadataName("System.Object")!,
                            TypeKind.Interface,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            new[] { interfaceDecl.GetLocation() },
                            new[] { interfaceDecl.GetReference() },
                            true,
                            isAbstract: true,
                            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                                interfaceDecl.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace())));

                        InitializeTypeParameters(interfaceSymbol, interfaceDecl.TypeParameterList);

                        if (!interfaceList.IsDefaultOrEmpty)
                            interfaceSymbol.SetInterfaces(interfaceList);

                        var interfaceBinder = new InterfaceDeclarationBinder(parentBinder, interfaceSymbol, interfaceDecl);
                        interfaceBinder.EnsureTypeParameterConstraintTypesResolved(interfaceSymbol.TypeParameters);
                        _binderCache[interfaceDecl] = interfaceBinder;
                        interfaceBinders.Add((interfaceDecl, interfaceBinder));
                        break;
                    }

                case ExtensionDeclarationSyntax extensionDecl:
                    {
                        var baseType = Compilation.GetTypeByMetadataName("System.Object");
                        var extensionAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            extensionDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));

                        ReportExternalTypeRedeclaration(
                            parentNamespace,
                            extensionDecl.Identifier,
                            extensionDecl.TypeParameterList?.Parameters.Count ?? 0,
                            parentBinder.Diagnostics);

                        var extensionSymbol = new SourceNamedTypeSymbol(
                            extensionDecl.Identifier.ValueText,
                            baseType!,
                            TypeKind.Class,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            new[] { extensionDecl.GetLocation() },
                            new[] { extensionDecl.GetReference() },
                            isSealed: true,
                            isAbstract: true,
                            declaredAccessibility: extensionAccessibility);

                        extensionSymbol.MarkAsExtensionContainer();

                        InitializeTypeParameters(extensionSymbol, extensionDecl.TypeParameterList);

                        var extensionBinder = new ExtensionDeclarationBinder(parentBinder, extensionSymbol, extensionDecl);
                        extensionBinder.EnsureTypeParameterConstraintTypesResolved(extensionSymbol.TypeParameters);
                        _binderCache[extensionDecl] = extensionBinder;

                        extensionBinders.Add((extensionDecl, extensionBinder));
                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            enumDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));
                        ReportExternalTypeRedeclaration(
                            parentNamespace,
                            enumDecl.Identifier,
                            arity: 0,
                            parentBinder.Diagnostics);

                        var enumSymbol = new SourceNamedTypeSymbol(
                            enumDecl.Identifier.ValueText,
                            Compilation.GetTypeByMetadataName("System.Enum"),
                            TypeKind.Enum,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            new[] { enumDecl.GetLocation() },
                            new[] { enumDecl.GetReference() },
                            true,
                            declaredAccessibility: enumAccessibility
                        );

                        var enumBinder = new EnumDeclarationBinder(parentBinder, enumSymbol, enumDecl);
                        _binderCache[enumDecl] = enumBinder;

                        int value = 0;
                        foreach (var enumMember in enumDecl.Members)
                        {
                            _ = new SourceFieldSymbol(
                                enumMember.Identifier.ValueText,
                                enumSymbol,
                                isStatic: true,
                                isLiteral: true,
                                constantValue: value++,
                                enumSymbol,
                                enumSymbol,
                                parentNamespace.AsSourceNamespace(),
                                new[] { enumMember.GetLocation() },
                                new[] { enumMember.GetReference() },
                                null,
                                declaredAccessibility: Accessibility.Public
                            );
                        }

                        break;
                    }
            }
        }

        foreach (var (unionDecl, unionBinder, unionSymbol) in unionBinders)
            RegisterUnionCases(unionDecl, unionBinder, unionSymbol);

        foreach (var (classDecl, classBinder) in classBinders)
        {
            RegisterClassMembers(classDecl, classBinder);
            classBinder.EnsureDefaultConstructor();
        }

        foreach (var (interfaceDecl, interfaceBinder) in interfaceBinders)
            RegisterInterfaceMembers(interfaceDecl, interfaceBinder);

        foreach (var (extensionDecl, extensionBinder) in extensionBinders)
            RegisterExtensionMembers(extensionDecl, extensionBinder);
    }
    private void RegisterUnionCases(UnionDeclarationSyntax unionDecl, UnionDeclarationBinder unionBinder, SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var namespaceSymbol = unionBinder.CurrentNamespace?.AsSourceNamespace()
            ?? unionSymbol.ContainingNamespace?.AsSourceNamespace();
        var caseSymbols = new List<IDiscriminatedUnionCaseSymbol>();
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var valueType = Compilation.GetSpecialType(SpecialType.System_ValueType);
        var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var objectToString = GetObjectToStringMethod();
        int ordinal = 0;

        void RegisterMember(SourceNamedTypeSymbol owner, ISymbol member)
        {
            if (!owner.GetMembers().Any(m => SymbolEqualityComparer.Default.Equals(m, member)))
                owner.AddMember(member);
        }

        void RegisterCaseMember(ISymbol member)
        {
            RegisterMember(unionSymbol, member);
            RegisterMember((SourceNamedTypeSymbol)member.ContainingType!, member);
        }

        var unionAccessibility = unionSymbol.DeclaredAccessibility;

        foreach (var caseClause in unionDecl.Cases)
        {
            var caseSymbol = new SourceDiscriminatedUnionCaseTypeSymbol(
                caseClause.Identifier.ValueText,
                ordinal++,
                valueType!,
                unionSymbol,
                unionSymbol,
                namespaceSymbol,
                [caseClause.GetLocation()],
                [caseClause.GetReference()],
                unionAccessibility);

            RegisterMember(unionSymbol, caseSymbol);

            var constructor = new SourceMethodSymbol(
                ".ctor",
                unitType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                caseSymbol,
                caseSymbol,
                namespaceSymbol,
                [caseClause.GetLocation()],
                [caseClause.GetReference()],
                isStatic: false,
                methodKind: MethodKind.Constructor,
                declaredAccessibility: Accessibility.Public);

            RegisterCaseMember(constructor);

            var parameters = new List<SourceParameterSymbol>();
            var seenOptionalParameter = false;

            if (caseClause.ParameterList is { } parameterList)
            {
                foreach (var parameterSyntax in parameterList.Parameters)
                {
                    var refKindTokenKind = parameterSyntax.RefKindKeyword?.Kind;
                    var refKind = refKindTokenKind switch
                    {
                        SyntaxKind.OutKeyword => RefKind.Out,
                        SyntaxKind.InKeyword => RefKind.In,
                        SyntaxKind.RefKeyword => RefKind.Ref,
                        _ => RefKind.None,
                    };

                    var typeSyntax = parameterSyntax.TypeAnnotation?.Type;
                    var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
                    var parameterType = typeSyntax is null
                        ? Compilation.ErrorTypeSymbol
                        : refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                            ? unionBinder.ResolveType(typeSyntax, refKindForType)
                            : unionBinder.ResolveType(typeSyntax);

                    var defaultResult = TypeMemberBinder.ProcessParameterDefault(
                        parameterSyntax,
                        parameterType,
                        parameterSyntax.Identifier.ValueText,
                        unionBinder.Diagnostics,
                        ref seenOptionalParameter);

                    var isMutable = parameterSyntax.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
                    var parameterSymbol = new SourceParameterSymbol(
                        parameterSyntax.Identifier.ValueText,
                        parameterType,
                        constructor,
                        caseSymbol,
                        namespaceSymbol,
                        [parameterSyntax.GetLocation()],
                        [parameterSyntax.GetReference()],
                        refKind,
                        defaultResult.HasExplicitDefaultValue,
                        defaultResult.ExplicitDefaultValue,
                        isMutable);

                    parameters.Add(parameterSymbol);

                    if (refKind == RefKind.None)
                    {
                        var parameterName = parameterSyntax.Identifier.ValueText;
                        var propertyName = GetUnionCasePropertyName(parameterName);

                        var backingField = new SourceFieldSymbol(
                            $"<{parameterSyntax.Identifier.ValueText}>k__BackingField",
                            parameterType,
                                isStatic: false,
                                isLiteral: false,
                                constantValue: null,
                                caseSymbol,
                                caseSymbol,
                                namespaceSymbol,
                                [parameterSyntax.GetLocation()],
                                [parameterSyntax.GetReference()],
                                new BoundParameterAccess(parameterSymbol),
                                declaredAccessibility: Accessibility.Private);

                        RegisterCaseMember(backingField);

                        var propertySymbol = new SourcePropertySymbol(
                            propertyName,
                            parameterType,
                            caseSymbol,
                            caseSymbol,
                            namespaceSymbol,
                            [parameterSyntax.GetLocation()],
                            [parameterSyntax.GetReference()],
                            declaredAccessibility: Accessibility.Private);

                        RegisterCaseMember(propertySymbol);

                        var getterSymbol = new SourceMethodSymbol(
                            $"get_{propertyName}",
                            parameterType,
                            ImmutableArray<SourceParameterSymbol>.Empty,
                            propertySymbol,
                            caseSymbol,
                            namespaceSymbol,
                            [parameterSyntax.GetLocation()],
                            [parameterSyntax.GetReference()],
                            isStatic: false,
                            methodKind: MethodKind.PropertyGet,
                            declaredAccessibility: Accessibility.Public);

                        RegisterCaseMember(getterSymbol);

                        propertySymbol.SetBackingField(backingField);
                        propertySymbol.SetAccessors(getterSymbol, null);
                    }
                }
            }

            constructor.SetParameters(parameters);
            caseSymbol.SetConstructorParameters(parameters);

            var caseToString = new SourceMethodSymbol(
                "ToString",
                stringType!,
                ImmutableArray<SourceParameterSymbol>.Empty,
                caseSymbol,
                caseSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                isOverride: true,
                declaredAccessibility: Accessibility.Public);

            RegisterCaseMember(caseToString);

            caseToString.SetOverriddenMethod(objectToString);
            RegisterUnionCaseSymbol(caseClause, caseSymbol);
            caseSymbols.Add(caseSymbol);

            var conversionMethod = new SourceMethodSymbol(
                "op_Implicit",
                unionSymbol,
                ImmutableArray<SourceParameterSymbol>.Empty,
                unionSymbol,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                isStatic: true,
                methodKind: MethodKind.Conversion,
                declaredAccessibility: Accessibility.Public);

            var conversionParameter = new SourceParameterSymbol(
                "value",
                caseSymbol,
                conversionMethod,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>());

            conversionMethod.SetParameters(new[] { conversionParameter });

            RegisterCaseMember(conversionMethod);

            var tryGetMethod = new SourceMethodSymbol(
                $"TryGet{caseClause.Identifier.ValueText}",
                boolType!,
                ImmutableArray<SourceParameterSymbol>.Empty,
                unionSymbol,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                declaredAccessibility: Accessibility.Public);

            var tryGetParameter = new SourceParameterSymbol(
                "value",
                caseSymbol,
                tryGetMethod,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                RefKind.Ref);

            tryGetMethod.SetParameters(new[] { tryGetParameter });

            RegisterCaseMember(tryGetMethod);
        }

        unionSymbol.SetCases(caseSymbols);
    }

    private static string GetUnionCasePropertyName(string parameterName)
    {
        if (string.IsNullOrEmpty(parameterName))
            return parameterName;

        if (char.IsUpper(parameterName[0]))
            return parameterName;

        Span<char> buffer = stackalloc char[parameterName.Length];
        parameterName.AsSpan().CopyTo(buffer);
        buffer[0] = char.ToUpperInvariant(buffer[0]);
        return new string(buffer);
    }

    private void ReportExternalTypeRedeclaration(
        INamespaceSymbol? namespaceSymbol,
        SyntaxToken identifier,
        int arity,
        DiagnosticBag diagnostics)
    {
        var mergedNamespace = GetMergedNamespace(namespaceSymbol);
        if (mergedNamespace is null)
            return;

        foreach (var member in mergedNamespace.GetMembers(identifier.ValueText).OfType<INamedTypeSymbol>())
        {
            if (member.Arity == arity && member.ContainingAssembly != Compilation.Assembly)
            {
                diagnostics.ReportTypeAlreadyDefined(identifier.ValueText, identifier.GetLocation());
                break;
            }
        }
    }

    private INamespaceSymbol? GetMergedNamespace(INamespaceSymbol? namespaceSymbol)
    {
        if (namespaceSymbol is null)
            return null;

        var merged = Compilation.GlobalNamespace;
        if (namespaceSymbol.IsGlobalNamespace)
            return merged;

        var namespaceName = namespaceSymbol.ToMetadataName();
        if (string.IsNullOrEmpty(namespaceName))
            return merged;

        foreach (var part in namespaceName.Split('.', StringSplitOptions.RemoveEmptyEntries))
        {
            merged = merged.LookupNamespace(part);

            if (merged is null)
                break;
        }

        return merged;
    }

    private (UnionDeclarationBinder Binder, SourceDiscriminatedUnionSymbol Symbol) RegisterUnionDeclaration(
        UnionDeclarationSyntax unionDecl,
        Binder parentBinder,
        ISymbol declaringSymbol,
        SourceNamespaceSymbol? namespaceSymbol)
    {
        var containingType = declaringSymbol as INamedTypeSymbol;
        var containingNamespace = declaringSymbol switch
        {
            INamespaceSymbol ns => ns,
            INamedTypeSymbol type => type.ContainingNamespace,
            _ => namespaceSymbol
        };

        var baseTypeSymbol = Compilation.GetSpecialType(SpecialType.System_ValueType);
        var unionAccessibility = AccessibilityUtilities.DetermineAccessibility(
            unionDecl.Modifiers,
            AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol));

        var unionSymbol = new SourceDiscriminatedUnionSymbol(
            unionDecl.Identifier.ValueText,
            baseTypeSymbol!,
            declaringSymbol,
            containingType,
            containingNamespace,
            [unionDecl.GetLocation()],
            [unionDecl.GetReference()],
            unionAccessibility);

        InitializeTypeParameters(unionSymbol, unionDecl.TypeParameterList);

        var unionBinder = new UnionDeclarationBinder(parentBinder, unionSymbol, unionDecl);
        unionBinder.EnsureTypeParameterConstraintTypesResolved(unionSymbol.TypeParameters);
        _binderCache[unionDecl] = unionBinder;

        namespaceSymbol ??= unionSymbol.ContainingNamespace?.AsSourceNamespace();

        var discriminatorField = new SourceFieldSymbol(
            "<Tag>",
            Compilation.GetSpecialType(SpecialType.System_Int32),
            isStatic: false,
            isLiteral: false,
            constantValue: 0,
            unionSymbol,
            unionSymbol,
            namespaceSymbol,
            [unionDecl.GetLocation()],
            [unionDecl.GetReference()],
            null,
            declaredAccessibility: Accessibility.Internal);

        var payloadField = new SourceFieldSymbol(
            "<Payload>",
            Compilation.GetSpecialType(SpecialType.System_Object),
            isStatic: false,
            isLiteral: false,
            constantValue: null,
            unionSymbol,
            unionSymbol,
            namespaceSymbol,
            [unionDecl.GetLocation()],
            [unionDecl.GetReference()],
            null,
            declaredAccessibility: Accessibility.Internal);

        var stringType = Compilation.GetSpecialType(SpecialType.System_String);
        var objectToString = GetObjectToStringMethod();

        var unionToString = new SourceMethodSymbol(
            "ToString",
            stringType!,
            ImmutableArray<SourceParameterSymbol>.Empty,
            unionSymbol,
            unionSymbol,
            namespaceSymbol,
            new[] { unionDecl.GetLocation() },
            Array.Empty<SyntaxReference>(),
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            isOverride: true,
            declaredAccessibility: Accessibility.Public);

        unionToString.SetOverriddenMethod(objectToString);
        unionSymbol.InitializeStorageFields(discriminatorField, payloadField);

        RegisterUnionSymbol(unionDecl, unionSymbol);

        return (unionBinder, unionSymbol);
    }

    private IMethodSymbol GetObjectToStringMethod()
    {
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        return objectType!
            .GetMembers("ToString")
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 0);
    }

    private void RegisterClassMembers(ClassDeclarationSyntax classDecl, ClassDeclarationBinder classBinder)
    {
        if (classDecl.ParameterList is not null)
            RegisterPrimaryConstructor(classDecl, classBinder);

        foreach (var member in classDecl.Members)
        {
            switch (member)
            {
                case FieldDeclarationSyntax fieldDecl:
                    var fieldBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    fieldBinder.BindFieldDeclaration(fieldDecl);
                    _binderCache[fieldDecl] = fieldBinder;
                    foreach (var decl in fieldDecl.Declaration.Declarators)
                        _binderCache[decl] = fieldBinder;
                    break;

                case MethodDeclarationSyntax methodDecl:
                    var memberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var methodBinder = memberBinder.BindMethodDeclaration(methodDecl);
                    _binderCache[methodDecl] = methodBinder;
                    break;

                case PropertyDeclarationSyntax propDecl:
                    var propMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var accessorBinders = propMemberBinder.BindPropertyDeclaration(propDecl);
                    _binderCache[propDecl] = propMemberBinder;
                    foreach (var kv in accessorBinders)
                        _binderCache[kv.Key] = kv.Value;
                    break;

                case IndexerDeclarationSyntax indexerDecl:
                    var indexerMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var indexerAccessorBinders = indexerMemberBinder.BindIndexerDeclaration(indexerDecl);
                    _binderCache[indexerDecl] = indexerMemberBinder;
                    foreach (var kv in indexerAccessorBinders)
                        _binderCache[kv.Key] = kv.Value;
                    break;

                case ConstructorDeclarationSyntax ctorDecl:
                    var ctorMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var ctorBinder = ctorMemberBinder.BindConstructorDeclaration(ctorDecl);
                    _binderCache[ctorDecl] = ctorBinder;
                    break;

                case NamedConstructorDeclarationSyntax ctorDecl:
                    var namedCtorMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var namedCtorBinder = namedCtorMemberBinder.BindNamedConstructorDeclaration(ctorDecl);
                    _binderCache[ctorDecl] = namedCtorBinder;
                    break;

                case ClassDeclarationSyntax nestedClass:
                    var parentType = (INamedTypeSymbol)classBinder.ContainingSymbol;
                    var nestedBaseType = Compilation.GetTypeByMetadataName("System.Object");
                    ImmutableArray<INamedTypeSymbol> nestedInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;
                    if (nestedClass.BaseList is not null)
                    {
                        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                        foreach (var t in nestedClass.BaseList.Types)
                        {
                            var resolved = classBinder.ResolveType(t) as INamedTypeSymbol;
                            if (resolved is null)
                                continue;

                            if (resolved.TypeKind == TypeKind.Interface)
                                builder.Add(resolved);
                            else
                                nestedBaseType = resolved;
                        }
                        if (builder.Count > 0)
                            nestedInterfaces = builder.ToImmutable();
                    }
                    var nestedAbstract = nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
                    var nestedSealed = !nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !nestedAbstract;
                    var nestedPartial = nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.PartialKeyword);
                    var nestedAccessibility = AccessibilityUtilities.DetermineAccessibility(
                        nestedClass.Modifiers,
                        AccessibilityUtilities.GetDefaultTypeAccessibility(parentType));

                    var nestedLocation = nestedClass.GetLocation();
                    var nestedReference = nestedClass.GetReference();

                    SourceNamedTypeSymbol nestedSymbol;
                    var isNewNestedSymbol = true;

                    if (parentType is SourceNamedTypeSymbol parentSourceType &&
                        parentSourceType.IsMemberDefined(nestedClass.Identifier.ValueText, out var existingNestedMember) &&
                        existingNestedMember is SourceNamedTypeSymbol existingNested &&
                        existingNested.TypeKind == TypeKind.Class)
                    {
                        var hadPartial = existingNested.HasPartialModifier;
                        var hadNonPartial = existingNested.HasNonPartialDeclaration;
                        var previouslyMixed = hadPartial && hadNonPartial;
                        var willBeMixed = (hadPartial || nestedPartial) && (hadNonPartial || !nestedPartial);

                        if (willBeMixed && !previouslyMixed)
                        {
                            classBinder.Diagnostics.ReportPartialTypeDeclarationMissingPartial(
                                nestedClass.Identifier.ValueText,
                                nestedClass.Identifier.GetLocation());
                        }
                        else if (hadNonPartial && !nestedPartial)
                        {
                            classBinder.Diagnostics.ReportTypeAlreadyDefined(
                                nestedClass.Identifier.ValueText,
                                nestedClass.Identifier.GetLocation());
                        }

                        existingNested.AddDeclaration(nestedLocation, nestedReference);
                        existingNested.UpdateDeclarationModifiers(nestedSealed, nestedAbstract);
                        existingNested.RegisterPartialModifier(nestedPartial);

                        nestedSymbol = existingNested;
                        isNewNestedSymbol = false;
                    }
                    else
                    {
                        nestedSymbol = new SourceNamedTypeSymbol(
                            nestedClass.Identifier.ValueText,
                            nestedBaseType!,
                            TypeKind.Class,
                            parentType,
                            parentType,
                            classBinder.CurrentNamespace!.AsSourceNamespace(),
                            [nestedLocation],
                            [nestedReference],
                            nestedSealed,
                            nestedAbstract,
                            declaredAccessibility: nestedAccessibility
                        );

                        nestedSymbol.RegisterPartialModifier(nestedPartial);
                    }

                    if (isNewNestedSymbol)
                        InitializeTypeParameters(nestedSymbol, nestedClass.TypeParameterList);

                    if (!nestedInterfaces.IsDefaultOrEmpty)
                        nestedSymbol.SetInterfaces(nestedInterfaces);

                    var nestedBinder = new ClassDeclarationBinder(classBinder, nestedSymbol, nestedClass);
                    nestedBinder.EnsureTypeParameterConstraintTypesResolved(nestedSymbol.TypeParameters);
                    _binderCache[nestedClass] = nestedBinder;
                    RegisterClassSymbol(nestedClass, nestedSymbol);
                    if (nestedClass.BaseList is not null && nestedBaseType!.IsSealed)
                        nestedBinder.Diagnostics.ReportCannotInheritFromSealedType(nestedBaseType.Name, nestedClass.BaseList.Types[0].GetLocation());
                    RegisterClassMembers(nestedClass, nestedBinder);
                    nestedBinder.EnsureDefaultConstructor();
                    break;

                case UnionDeclarationSyntax nestedUnion:
                    {
                        var declaringSymbol = (ISymbol)classBinder.ContainingSymbol;
                        var namespaceSymbol = classBinder.CurrentNamespace?.AsSourceNamespace();
                        var (unionBinder, unionSymbol) = RegisterUnionDeclaration(nestedUnion, classBinder, declaringSymbol, namespaceSymbol);
                        RegisterUnionCases(nestedUnion, unionBinder, unionSymbol);
                        break;
                    }

                case InterfaceDeclarationSyntax nestedInterface:
                    var parentForInterface = (INamedTypeSymbol)classBinder.ContainingSymbol;
                    ImmutableArray<INamedTypeSymbol> parentInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;
                    if (nestedInterface.BaseList is not null)
                    {
                        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                        foreach (var t in nestedInterface.BaseList.Types)
                        {
                            if (classBinder.ResolveType(t) is INamedTypeSymbol resolved && resolved.TypeKind == TypeKind.Interface)
                                builder.Add(resolved);
                        }
                        if (builder.Count > 0)
                            parentInterfaces = builder.ToImmutable();
                    }
                    var nestedInterfaceSymbol = new SourceNamedTypeSymbol(
                        nestedInterface.Identifier.ValueText,
                        Compilation.GetTypeByMetadataName("System.Object")!,
                        TypeKind.Interface,
                        parentForInterface,
                        parentForInterface,
                        classBinder.CurrentNamespace!.AsSourceNamespace(),
                        [nestedInterface.GetLocation()],
                        [nestedInterface.GetReference()],
                        true,
                        isAbstract: true,
                        declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                            nestedInterface.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentForInterface))
                    );

                    InitializeTypeParameters(nestedInterfaceSymbol, nestedInterface.TypeParameterList);

                    if (!parentInterfaces.IsDefaultOrEmpty)
                        nestedInterfaceSymbol.SetInterfaces(parentInterfaces);
                    var nestedInterfaceBinder = new InterfaceDeclarationBinder(classBinder, nestedInterfaceSymbol, nestedInterface);
                    nestedInterfaceBinder.EnsureTypeParameterConstraintTypesResolved(nestedInterfaceSymbol.TypeParameters);
                    _binderCache[nestedInterface] = nestedInterfaceBinder;
                    RegisterInterfaceMembers(nestedInterface, nestedInterfaceBinder);
                    break;

                case EnumDeclarationSyntax enumDecl:
                    var parentTypeForEnum = (INamedTypeSymbol)classBinder.ContainingSymbol;
                    var enumSymbol = new SourceNamedTypeSymbol(
                        enumDecl.Identifier.ValueText,
                        Compilation.GetTypeByMetadataName("System.Enum"),
                        TypeKind.Enum,
                        parentTypeForEnum,
                        parentTypeForEnum,
                        classBinder.CurrentNamespace!.AsSourceNamespace(),
                        [enumDecl.GetLocation()],
                        [enumDecl.GetReference()],
                        true,
                        declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                            enumDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentTypeForEnum))
                    );

                    var enumBinder = new EnumDeclarationBinder(classBinder, enumSymbol, enumDecl);
                    _binderCache[enumDecl] = enumBinder;

                    int value = 0;
                    foreach (var enumMember in enumDecl.Members)
                    {
                        _ = new SourceFieldSymbol(
                            enumMember.Identifier.ValueText,
                            enumSymbol,
                            isStatic: true,
                            isLiteral: true,
                            constantValue: value++,
                            enumSymbol,
                            enumSymbol,
                            classBinder.CurrentNamespace!.AsSourceNamespace(),
                            [enumMember.GetLocation()],
                            [enumMember.GetReference()],
                            null,
                            declaredAccessibility: Accessibility.Public
                        );
                    }
                    break;
            }
        }

        classBinder.EnsureDefaultConstructor();
    }

    private void RegisterInterfaceMembers(InterfaceDeclarationSyntax interfaceDecl, InterfaceDeclarationBinder interfaceBinder)
    {
        foreach (var member in interfaceDecl.Members)
        {
            switch (member)
            {
                case MethodDeclarationSyntax methodDecl:
                    {
                        var memberBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var methodBinder = memberBinder.BindMethodDeclaration(methodDecl);
                        _binderCache[methodDecl] = methodBinder;
                        break;
                    }
                case PropertyDeclarationSyntax propertyDecl:
                    {
                        var propertyBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var accessorBinders = propertyBinder.BindPropertyDeclaration(propertyDecl);
                        _binderCache[propertyDecl] = propertyBinder;
                        foreach (var kv in accessorBinders)
                            _binderCache[kv.Key] = kv.Value;
                        break;
                    }
                case IndexerDeclarationSyntax indexerDecl:
                    {
                        var indexerBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var accessorBinders = indexerBinder.BindIndexerDeclaration(indexerDecl);
                        _binderCache[indexerDecl] = indexerBinder;
                        foreach (var kv in accessorBinders)
                            _binderCache[kv.Key] = kv.Value;
                        break;
                    }
                case InterfaceDeclarationSyntax nestedInterface:
                    {
                        var parentInterface = (INamedTypeSymbol)interfaceBinder.ContainingSymbol;
                        ImmutableArray<INamedTypeSymbol> parentInterfaces = ImmutableArray<INamedTypeSymbol>.Empty;
                        if (nestedInterface.BaseList is not null)
                        {
                            var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                            foreach (var t in nestedInterface.BaseList.Types)
                            {
                                if (interfaceBinder.ResolveType(t) is INamedTypeSymbol resolved && resolved.TypeKind == TypeKind.Interface)
                                    builder.Add(resolved);
                            }

                            if (builder.Count > 0)
                                parentInterfaces = builder.ToImmutable();
                        }

                        var nestedInterfaceSymbol = new SourceNamedTypeSymbol(
                            nestedInterface.Identifier.ValueText,
                            Compilation.GetTypeByMetadataName("System.Object")!,
                            TypeKind.Interface,
                            parentInterface,
                            parentInterface,
                            interfaceBinder.CurrentNamespace!.AsSourceNamespace(),
                            [nestedInterface.GetLocation()],
                            [nestedInterface.GetReference()],
                            true,
                            isAbstract: true,
                            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                                nestedInterface.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentInterface)));

                        if (!parentInterfaces.IsDefaultOrEmpty)
                            nestedInterfaceSymbol.SetInterfaces(parentInterfaces);

                        var nestedInterfaceBinder = new InterfaceDeclarationBinder(interfaceBinder, nestedInterfaceSymbol, nestedInterface);
                        nestedInterfaceBinder.EnsureTypeParameterConstraintTypesResolved(nestedInterfaceSymbol.TypeParameters);
                        _binderCache[nestedInterface] = nestedInterfaceBinder;
                        RegisterInterfaceMembers(nestedInterface, nestedInterfaceBinder);
                        break;
                    }
            }
        }
    }

    private void RegisterExtensionMembers(ExtensionDeclarationSyntax extensionDecl, ExtensionDeclarationBinder extensionBinder)
    {
        foreach (var member in extensionDecl.Members)
        {
            switch (member)
            {
                case MethodDeclarationSyntax methodDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var methodBinder = memberBinder.BindMethodDeclaration(methodDecl);
                        _binderCache[methodDecl] = methodBinder;
                        break;
                    }

                case PropertyDeclarationSyntax propertyDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var accessorBinders = memberBinder.BindPropertyDeclaration(propertyDecl);
                        _binderCache[propertyDecl] = memberBinder;
                        foreach (var kv in accessorBinders)
                            _binderCache[kv.Key] = kv.Value;
                        break;
                    }
            }
        }
    }

    private static void InitializeTypeParameters(SourceNamedTypeSymbol typeSymbol, TypeParameterListSyntax? typeParameterList)
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            return;

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
        var ordinal = 0;

        foreach (var parameter in typeParameterList.Parameters)
        {
            var (constraintKind, constraintTypeReferences) = AnalyzeTypeParameterConstraints(parameter);
            var variance = GetDeclaredVariance(parameter);

            var typeParameter = new SourceTypeParameterSymbol(
                parameter.Identifier.ValueText,
                typeSymbol,
                typeSymbol,
                typeSymbol.ContainingNamespace,
                [parameter.GetLocation()],
                [parameter.GetReference()],
                ordinal++,
                constraintKind,
                constraintTypeReferences,
                variance);

            builder.Add(typeParameter);
        }

        typeSymbol.SetTypeParameters(builder.MoveToImmutable());
    }

    private static (TypeParameterConstraintKind constraintKind, ImmutableArray<SyntaxReference> constraintTypeReferences) AnalyzeTypeParameterConstraints(TypeParameterSyntax parameter)
    {
        if (parameter.ColonToken is null)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        var constraints = parameter.Constraints;
        if (constraints.Count == 0)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        var constraintKind = TypeParameterConstraintKind.None;
        var typeConstraintReferences = ImmutableArray.CreateBuilder<SyntaxReference>();

        foreach (var constraint in constraints)
        {
            switch (constraint)
            {
                case ClassConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ReferenceType;
                    break;
                case StructConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ValueType;
                    break;
                case TypeConstraintSyntax typeConstraint:
                    constraintKind |= TypeParameterConstraintKind.TypeConstraint;
                    typeConstraintReferences.Add(typeConstraint.GetReference());
                    break;
            }
        }

        return (constraintKind, typeConstraintReferences.ToImmutable());
    }

    private static VarianceKind GetDeclaredVariance(TypeParameterSyntax parameter)
    {
        return parameter.VarianceKeyword?.Kind switch
        {
            SyntaxKind.OutKeyword => VarianceKind.Out,
            SyntaxKind.InKeyword => VarianceKind.In,
            _ => VarianceKind.None,
        };
    }

    private void RegisterPrimaryConstructor(ClassDeclarationSyntax classDecl, ClassDeclarationBinder classBinder)
    {
        var classSymbol = (SourceNamedTypeSymbol)classBinder.ContainingSymbol;
        var namespaceSymbol = classBinder.CurrentNamespace!.AsSourceNamespace();
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);

        var constructorSymbol = new SourceMethodSymbol(
            ".ctor",
            unitType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            classSymbol,
            classSymbol,
            namespaceSymbol,
            [classDecl.GetLocation()],
            [classDecl.GetReference()],
            isStatic: false,
            methodKind: MethodKind.Constructor,
            declaredAccessibility: classSymbol.DeclaredAccessibility);

        var parameters = new List<SourceParameterSymbol>();

        var seenOptionalParameter = false;
        foreach (var parameterSyntax in classDecl.ParameterList!.Parameters)
        {
            var refKindTokenKind = parameterSyntax.RefKindKeyword?.Kind;
            var refKind = refKindTokenKind switch
            {
                SyntaxKind.OutKeyword => RefKind.Out,
                SyntaxKind.InKeyword => RefKind.In,
                SyntaxKind.RefKeyword => RefKind.Ref,
                _ => RefKind.None,
            };

            var typeSyntax = parameterSyntax.TypeAnnotation?.Type;
            var refKindForType = refKind == RefKind.None && typeSyntax is ByRefTypeSyntax ? RefKind.Ref : refKind;
            var parameterType = typeSyntax is null
                ? Compilation.ErrorTypeSymbol
                : refKindForType is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                    ? classBinder.ResolveType(typeSyntax, refKindForType)
                    : classBinder.ResolveType(typeSyntax);

            var defaultResult = TypeMemberBinder.ProcessParameterDefault(
                parameterSyntax,
                parameterType,
                parameterSyntax.Identifier.ValueText,
                classBinder.Diagnostics,
                ref seenOptionalParameter);
            var isMutable = parameterSyntax.BindingKeyword?.Kind == SyntaxKind.VarKeyword;
            var parameterSymbol = new SourceParameterSymbol(
                parameterSyntax.Identifier.ValueText,
                parameterType,
                constructorSymbol,
                classSymbol,
                namespaceSymbol,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                refKind,
                defaultResult.HasExplicitDefaultValue,
                defaultResult.ExplicitDefaultValue,
                isMutable);

            parameters.Add(parameterSymbol);

            if (refKind == RefKind.None)
            {
                _ = new SourceFieldSymbol(
                    parameterSyntax.Identifier.ValueText,
                    parameterType,
                    isStatic: false,
                    isLiteral: false,
                    constantValue: null,
                    classSymbol,
                    classSymbol,
                    namespaceSymbol,
                    [parameterSyntax.GetLocation()],
                    [parameterSyntax.GetReference()],
                    new BoundParameterAccess(parameterSymbol),
                    declaredAccessibility: Accessibility.Private);
            }
        }

        constructorSymbol.SetParameters(parameters);
    }

    internal BoundNode? TryGetCachedBoundNode(SyntaxNode node)
    => _boundNodeCache.TryGetValue(node, out var bound) ? bound : null;

    internal void CacheBoundNode(SyntaxNode node, BoundNode bound)
        => _boundNodeCache[node] = bound;

    internal void RemoveCachedBoundNode(SyntaxNode node)
        => _boundNodeCache.Remove(node);

    private readonly Dictionary<ClassDeclarationSyntax, SourceNamedTypeSymbol> _classSymbols = new();

    internal void RegisterClassSymbol(ClassDeclarationSyntax node, SourceNamedTypeSymbol symbol)
        => _classSymbols[node] = symbol;

    internal SourceNamedTypeSymbol GetClassSymbol(ClassDeclarationSyntax node)
        => _classSymbols[node];

    private readonly Dictionary<UnionDeclarationSyntax, SourceDiscriminatedUnionSymbol> _unionSymbols = new();
    private readonly Dictionary<UnionCaseClauseSyntax, SourceDiscriminatedUnionCaseTypeSymbol> _unionCaseSymbols = new();

    internal void RegisterUnionSymbol(UnionDeclarationSyntax node, SourceDiscriminatedUnionSymbol symbol)
        => _unionSymbols[node] = symbol;

    internal SourceDiscriminatedUnionSymbol GetUnionSymbol(UnionDeclarationSyntax node)
        => _unionSymbols[node];

    internal void RegisterUnionCaseSymbol(UnionCaseClauseSyntax node, SourceDiscriminatedUnionCaseTypeSymbol symbol)
        => _unionCaseSymbols[node] = symbol;

    internal SourceDiscriminatedUnionCaseTypeSymbol GetUnionCaseSymbol(UnionCaseClauseSyntax node)
        => _unionCaseSymbols[node];
}
