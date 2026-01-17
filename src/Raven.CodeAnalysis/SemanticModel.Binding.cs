using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly Dictionary<LabeledStatementSyntax, ILabelSymbol> _labelDeclarations = new();
    private readonly Dictionary<ILabelSymbol, LabeledStatementSyntax> _labelSyntax = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<string, List<ILabelSymbol>> _labelsByName = new(StringComparer.Ordinal);
    private readonly Dictionary<GotoStatementSyntax, ILabelSymbol> _gotoTargets = new();
    private readonly Dictionary<AttributeSyntax, AttributeData?> _attributeCache = new();

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

        var topLevelMainFunctions = bindableGlobals
            .Where(static g => g.Statement is FunctionStatementSyntax { Identifier.ValueText: "Main" })
            .ToArray();

        if (topLevelMainFunctions.Length > 0)
        {
            foreach (var statement in bindableGlobals)
            {
                if (statement.Statement is FunctionStatementSyntax { Identifier.ValueText: "Main" })
                    continue;

                if (statement.Statement is FunctionStatementSyntax)
                    continue;

                parentBinder.Diagnostics.ReportTopLevelStatementsDisallowedWithMainFunction(statement.GetLocation());
            }
        }

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

    private static string MangleUnnamedExtensionName(ExtensionDeclarationSyntax extensionDecl)
    {
        // Stable, deterministic mangling for unnamed extensions.
        // The name is not intended to be user-visible; it exists to provide a unique symbol identity
        // within the assembly for binding, caching, and metadata emission.
        var loc = extensionDecl.GetLocation();
        var span = loc.SourceSpan;

        var filePath = extensionDecl.SyntaxTree?.FilePath ?? string.Empty;

        // FNV-1a 32-bit hash for stability across processes/runs.
        unchecked
        {
            uint hash = 2166136261;
            foreach (var ch in filePath)
            {
                hash ^= ch;
                hash *= 16777619;
            }

            // Include span to avoid collisions within the same file.
            hash ^= (uint)span.Start;
            hash *= 16777619;
            hash ^= (uint)span.Length;
            hash *= 16777619;

            return $"__ext${hash:x8}_{span.Start}_{span.Length}";
        }
    }

    private void ReportExternalTypeRedeclaration(
        INamespaceSymbol? namespaceSymbol,
        string name,
        Location location,
        int arity,
        DiagnosticBag diagnostics)
    {
        var mergedNamespace = GetMergedNamespace(namespaceSymbol);
        if (mergedNamespace is null)
            return;

        foreach (var member in mergedNamespace.GetMembers(name).OfType<INamedTypeSymbol>())
        {
            if (member.Arity == arity && member.ContainingAssembly != Compilation.Assembly)
            {
                diagnostics.ReportTypeAlreadyDefined(name, location);
                break;
            }
        }
    }

    private void RegisterNamespaceMembers(SyntaxNode containerNode, Binder parentBinder, INamespaceSymbol parentNamespace)
    {
        var classBinders = new List<(ClassDeclarationSyntax Syntax, ClassDeclarationBinder Binder)>();
        var interfaceBinders = new List<(InterfaceDeclarationSyntax Syntax, InterfaceDeclarationBinder Binder)>();
        var extensionBinders = new List<(ExtensionDeclarationSyntax Syntax, ExtensionDeclarationBinder Binder)>();
        var unionBinders = new List<(UnionDeclarationSyntax Syntax, UnionDeclarationBinder Binder, SourceDiscriminatedUnionSymbol Symbol)>();

        var classSymbols = new Dictionary<ClassDeclarationSyntax, SourceNamedTypeSymbol>();
        var interfaceSymbols = new Dictionary<InterfaceDeclarationSyntax, SourceNamedTypeSymbol>();
        var extensionSymbols = new Dictionary<ExtensionDeclarationSyntax, SourceNamedTypeSymbol>();
        var enumSymbols = new Dictionary<EnumDeclarationSyntax, SourceNamedTypeSymbol>();
        var unionSymbols = new Dictionary<UnionDeclarationSyntax, SourceDiscriminatedUnionSymbol>();

        var objectType = Compilation.GetTypeByMetadataName("System.Object");

        foreach (var member in containerNode.ChildNodes())
        {
            switch (member)
            {
                case ClassDeclarationSyntax classDecl:
                    {
                        var isStatic = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
                        var isAbstract = isStatic || classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
                        var isSealed = isStatic || (!classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !isAbstract);
                        var isPartial = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.PartialKeyword);
                        var isRecord = classDecl.Modifiers.Any(m => m.Kind == SyntaxKind.RecordKeyword);
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
                            existingType.UpdateDeclarationModifiers(isSealed, isAbstract, isStatic);
                            existingType.RegisterPartialModifier(isPartial);
                            existingType.RegisterRecordModifier(isRecord);

                            classSymbol = existingType;
                            isNewSymbol = false;
                        }
                        else
                        {
                            classSymbol = new SourceNamedTypeSymbol(
                                classDecl.Identifier.ValueText,
                                objectType!,
                                TypeKind.Class,
                                parentNamespace.AsSourceNamespace(),
                                null,
                                parentNamespace.AsSourceNamespace(),
                                new[] { declarationLocation },
                                new[] { declarationReference },
                                isSealed,
                                isAbstract,
                                isStatic,
                                declaredAccessibility: typeAccessibility);

                            classSymbol.RegisterPartialModifier(isPartial);
                            classSymbol.RegisterRecordModifier(isRecord);
                        }

                        if (isNewSymbol)
                            InitializeTypeParameters(classSymbol, classDecl.TypeParameterList, classDecl.ConstraintClauses);

                        classSymbols[classDecl] = classSymbol;
                        break;
                    }

                case InterfaceDeclarationSyntax interfaceDecl:
                    {
                        ReportExternalTypeRedeclaration(
                            parentNamespace,
                            interfaceDecl.Identifier,
                            interfaceDecl.TypeParameterList?.Parameters.Count ?? 0,
                            parentBinder.Diagnostics);

                        var interfaceSymbol = new SourceNamedTypeSymbol(
                            interfaceDecl.Identifier.ValueText,
                            objectType!,
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

                        InitializeTypeParameters(interfaceSymbol, interfaceDecl.TypeParameterList, interfaceDecl.ConstraintClauses);

                        interfaceSymbols[interfaceDecl] = interfaceSymbol;
                        break;
                    }

                case ExtensionDeclarationSyntax extensionDecl:
                    {
                        var extensionAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            extensionDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));

                        if (extensionAccessibility == Accessibility.Public &&
                            extensionDecl.Identifier.Kind == SyntaxKind.None)
                        {
                            parentBinder.Diagnostics.ReportPublicExtensionRequiresIdentifier(
                                extensionDecl.GetLocation());
                        }

                        var extensionName = extensionDecl.Identifier.Kind == SyntaxKind.None
                            ? MangleUnnamedExtensionName(extensionDecl)
                            : extensionDecl.Identifier.ValueText;

                        ReportExternalTypeRedeclaration(
                            parentNamespace,
                            extensionName,
                            extensionDecl.GetLocation(),
                            extensionDecl.TypeParameterList?.Parameters.Count ?? 0,
                            parentBinder.Diagnostics);

                        var extensionSymbol = new SourceNamedTypeSymbol(
                            extensionName,
                            objectType!,
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
                        InitializeTypeParameters(extensionSymbol, extensionDecl.TypeParameterList, extensionDecl.ConstraintClauses);

                        extensionSymbols[extensionDecl] = extensionSymbol;
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

                        var enumUnderlyingType = ResolveEnumUnderlyingType(enumDecl, parentBinder);
                        enumSymbol.SetEnumUnderlyingType(enumUnderlyingType);

                        enumSymbols[enumDecl] = enumSymbol;
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

                        var unionSymbol = new SourceDiscriminatedUnionSymbol(
                            unionDecl.Identifier.ValueText,
                            Compilation.GetSpecialType(SpecialType.System_ValueType)!,
                            declaringSymbol,
                            null,
                            namespaceSymbol,
                            [unionDecl.GetLocation()],
                            [unionDecl.GetReference()],
                            AccessibilityUtilities.DetermineAccessibility(
                                unionDecl.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol)));

                        InitializeTypeParameters(unionSymbol, unionDecl.TypeParameterList, unionDecl.ConstraintClauses);

                        unionSymbols[unionDecl] = unionSymbol;
                        break;
                    }
            }
        }

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
                        var classSymbol = classSymbols[classDecl];
                        var baseTypeSymbol = objectType;
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

                        if (classSymbol is SourceNamedTypeSymbol recordSymbol && recordSymbol.IsRecord)
                        {
                            if (Compilation.GetTypeByMetadataName("System.IEquatable`1") is INamedTypeSymbol equatableDefinition)
                            {
                                var equatableType = (INamedTypeSymbol)equatableDefinition.Construct(classSymbol);
                                if (interfaceList.IsDefaultOrEmpty)
                                {
                                    interfaceList = ImmutableArray.Create(equatableType);
                                }
                                else if (!interfaceList.Any(i => SymbolEqualityComparer.Default.Equals(i, equatableType)))
                                {
                                    interfaceList = interfaceList.Add(equatableType);
                                }
                            }
                        }

                        if (baseTypeSymbol is not null &&
                            !SymbolEqualityComparer.Default.Equals(classSymbol.BaseType, baseTypeSymbol) &&
                            SymbolEqualityComparer.Default.Equals(classSymbol.BaseType, objectType))
                        {
                            classSymbol.SetBaseType(baseTypeSymbol);
                        }

                        if (!interfaceList.IsDefaultOrEmpty)
                        {
                            classSymbol.SetInterfaces(MergeInterfaces(classSymbol.Interfaces, interfaceList));
                        }

                        var classBinder = new ClassDeclarationBinder(parentBinder, classSymbol, classDecl);
                        classBinder.EnsureTypeParameterConstraintTypesResolved(classSymbol.TypeParameters);
                        _binderCache[classDecl] = classBinder;
                        RegisterClassSymbol(classDecl, classSymbol);
                        if (classDecl.BaseList is not null && baseTypeSymbol?.IsStatic == true)
                        {
                            classBinder.Diagnostics.ReportStaticTypeCannotBeInherited(
                                baseTypeSymbol.Name,
                                classDecl.BaseList.Types[0].GetLocation());
                        }
                        else if (classDecl.BaseList is not null && baseTypeSymbol?.IsSealed == true)
                        {
                            classBinder.Diagnostics.ReportCannotInheritFromSealedType(
                                baseTypeSymbol.Name,
                                classDecl.BaseList.Types[0].GetLocation());
                        }

                        classBinders.Add((classDecl, classBinder));
                        break;
                    }

                case UnionDeclarationSyntax unionDecl:
                    {
                        var declaringSymbol = (ISymbol)(parentNamespace.AsSourceNamespace() ?? parentNamespace);
                        var namespaceSymbol = parentNamespace.AsSourceNamespace();
                        var unionSymbol = unionSymbols[unionDecl];
                        var (unionBinder, resolvedSymbol) = RegisterUnionDeclaration(
                            unionDecl,
                            parentBinder,
                            declaringSymbol,
                            namespaceSymbol,
                            unionSymbol);
                        unionBinders.Add((unionDecl, unionBinder, resolvedSymbol));
                        break;
                    }

                case InterfaceDeclarationSyntax interfaceDecl:
                    {
                        var interfaceSymbol = interfaceSymbols[interfaceDecl];
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

                        if (!interfaceList.IsDefaultOrEmpty)
                            interfaceSymbol.SetInterfaces(MergeInterfaces(interfaceSymbol.Interfaces, interfaceList));

                        var interfaceBinder = new InterfaceDeclarationBinder(parentBinder, interfaceSymbol, interfaceDecl);
                        interfaceBinder.EnsureTypeParameterConstraintTypesResolved(interfaceSymbol.TypeParameters);
                        _binderCache[interfaceDecl] = interfaceBinder;
                        interfaceBinders.Add((interfaceDecl, interfaceBinder));
                        break;
                    }

                case ExtensionDeclarationSyntax extensionDecl:
                    {
                        var extensionSymbol = extensionSymbols[extensionDecl];
                        var extensionBinder = new ExtensionDeclarationBinder(parentBinder, extensionSymbol, extensionDecl);
                        extensionBinder.EnsureTypeParameterConstraintTypesResolved(extensionSymbol.TypeParameters);
                        _binderCache[extensionDecl] = extensionBinder;

                        extensionBinders.Add((extensionDecl, extensionBinder));
                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumSymbol = enumSymbols[enumDecl];
                        var enumBinder = new EnumDeclarationBinder(parentBinder, enumSymbol, enumDecl);
                        _binderCache[enumDecl] = enumBinder;

                        RegisterEnumMembers(enumDecl, enumBinder, enumSymbol, parentNamespace.AsSourceNamespace());
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

        var checkedClasses = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var (classDecl, classBinder) in classBinders)
        {
            var classSymbol = (INamedTypeSymbol)classBinder.ContainingSymbol;
            if (!checkedClasses.Add(classSymbol))
                continue;

            ReportMissingAbstractBaseMembers(classSymbol, classDecl, classBinder.Diagnostics);
        }

        foreach (var (interfaceDecl, interfaceBinder) in interfaceBinders)
            RegisterInterfaceMembers(interfaceDecl, interfaceBinder);

        foreach (var (extensionDecl, extensionBinder) in extensionBinders)
            RegisterExtensionMembers(extensionDecl, extensionBinder);

        static ImmutableArray<INamedTypeSymbol> MergeInterfaces(
            ImmutableArray<INamedTypeSymbol> existing,
            ImmutableArray<INamedTypeSymbol> additional)
        {
            if (existing.IsDefaultOrEmpty)
                return additional;

            var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
            var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
            foreach (var type in existing)
                if (seen.Add(type))
                    builder.Add(type);

            foreach (var type in additional)
                if (seen.Add(type))
                    builder.Add(type);

            return builder.ToImmutable();
        }
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
                            isMutable: parameterSymbol.IsMutable,
                            isConst: false,
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
                caseSymbol, // Should be nullable?
                tryGetMethod,
                unionSymbol,
                namespaceSymbol,
                new[] { caseClause.GetLocation() },
                Array.Empty<SyntaxReference>(),
                RefKind.Out);

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

    private (UnionDeclarationBinder Binder, SourceDiscriminatedUnionSymbol Symbol) RegisterUnionDeclaration(
        UnionDeclarationSyntax unionDecl,
        Binder parentBinder,
        ISymbol declaringSymbol,
        SourceNamespaceSymbol? namespaceSymbol,
        SourceDiscriminatedUnionSymbol? existingSymbol = null)
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

        var unionSymbol = existingSymbol ?? new SourceDiscriminatedUnionSymbol(
            unionDecl.Identifier.ValueText,
            baseTypeSymbol!,
            declaringSymbol,
            containingType,
            containingNamespace,
            [unionDecl.GetLocation()],
            [unionDecl.GetReference()],
            unionAccessibility);

        if (unionSymbol.TypeParameters.IsDefaultOrEmpty)
            InitializeTypeParameters(unionSymbol, unionDecl.TypeParameterList, unionDecl.ConstraintClauses);

        var unionBinder = new UnionDeclarationBinder(parentBinder, unionSymbol, unionDecl);
        unionBinder.EnsureTypeParameterConstraintTypesResolved(unionSymbol.TypeParameters);
        _binderCache[unionDecl] = unionBinder;

        namespaceSymbol ??= unionSymbol.ContainingNamespace?.AsSourceNamespace();

        var discriminatorField = new SourceFieldSymbol(
            "<Tag>",
            Compilation.GetSpecialType(SpecialType.System_Int32),
            isStatic: false,
            isMutable: true,
            isConst: false,
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
            isMutable: true,
            isConst: false,
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

        var nestedClassBinders = new List<(ClassDeclarationSyntax Syntax, ClassDeclarationBinder Binder)>();
        var nestedClassSymbols = new Dictionary<ClassDeclarationSyntax, SourceNamedTypeSymbol>();
        var nestedInterfaceSymbols = new Dictionary<InterfaceDeclarationSyntax, SourceNamedTypeSymbol>();
        var nestedEnumSymbols = new Dictionary<EnumDeclarationSyntax, SourceNamedTypeSymbol>();
        var nestedUnionSymbols = new Dictionary<UnionDeclarationSyntax, SourceDiscriminatedUnionSymbol>();
        var objectType = Compilation.GetTypeByMetadataName("System.Object");
        var parentType = (INamedTypeSymbol)classBinder.ContainingSymbol;

        foreach (var member in classDecl.Members)
        {
            switch (member)
            {
                case ClassDeclarationSyntax nestedClass:
                    {
                        var nestedStatic = nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
                        var nestedAbstract = nestedStatic || nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
                        var nestedSealed = nestedStatic || (!nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !nestedAbstract);
                        var nestedPartial = nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.PartialKeyword);
                        var nestedRecord = nestedClass.Modifiers.Any(m => m.Kind == SyntaxKind.RecordKeyword);
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
                            existingNested.UpdateDeclarationModifiers(nestedSealed, nestedAbstract, nestedStatic);
                            existingNested.RegisterPartialModifier(nestedPartial);
                            existingNested.RegisterRecordModifier(nestedRecord);

                            nestedSymbol = existingNested;
                            isNewNestedSymbol = false;
                        }
                        else
                        {
                            nestedSymbol = new SourceNamedTypeSymbol(
                                nestedClass.Identifier.ValueText,
                                objectType!,
                                TypeKind.Class,
                                parentType,
                                parentType,
                                classBinder.CurrentNamespace!.AsSourceNamespace(),
                                [nestedLocation],
                                [nestedReference],
                                nestedSealed,
                                nestedAbstract,
                                nestedStatic,
                                declaredAccessibility: nestedAccessibility
                            );

                            nestedSymbol.RegisterPartialModifier(nestedPartial);
                            nestedSymbol.RegisterRecordModifier(nestedRecord);
                        }

                        if (isNewNestedSymbol)
                            InitializeTypeParameters(nestedSymbol, nestedClass.TypeParameterList, nestedClass.ConstraintClauses);

                        nestedClassSymbols[nestedClass] = nestedSymbol;
                        break;
                    }

                case InterfaceDeclarationSyntax nestedInterface:
                    {
                        var nestedInterfaceSymbol = new SourceNamedTypeSymbol(
                            nestedInterface.Identifier.ValueText,
                            objectType!,
                            TypeKind.Interface,
                            parentType,
                            parentType,
                            classBinder.CurrentNamespace!.AsSourceNamespace(),
                            [nestedInterface.GetLocation()],
                            [nestedInterface.GetReference()],
                            true,
                            isAbstract: true,
                            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                                nestedInterface.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentType))
                        );

                        InitializeTypeParameters(nestedInterfaceSymbol, nestedInterface.TypeParameterList, nestedInterface.ConstraintClauses);
                        nestedInterfaceSymbols[nestedInterface] = nestedInterfaceSymbol;
                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumSymbol = new SourceNamedTypeSymbol(
                            enumDecl.Identifier.ValueText,
                            Compilation.GetTypeByMetadataName("System.Enum"),
                            TypeKind.Enum,
                            parentType,
                            parentType,
                            classBinder.CurrentNamespace!.AsSourceNamespace(),
                            [enumDecl.GetLocation()],
                            [enumDecl.GetReference()],
                            true,
                            declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                                enumDecl.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentType))
                        );

                        var enumUnderlyingType = ResolveEnumUnderlyingType(enumDecl, classBinder);
                        enumSymbol.SetEnumUnderlyingType(enumUnderlyingType);

                        nestedEnumSymbols[enumDecl] = enumSymbol;
                        break;
                    }

                case UnionDeclarationSyntax nestedUnion:
                    {
                        var unionSymbol = new SourceDiscriminatedUnionSymbol(
                            nestedUnion.Identifier.ValueText,
                            Compilation.GetSpecialType(SpecialType.System_ValueType)!,
                            parentType,
                            parentType,
                            classBinder.CurrentNamespace!.AsSourceNamespace(),
                            [nestedUnion.GetLocation()],
                            [nestedUnion.GetReference()],
                            AccessibilityUtilities.DetermineAccessibility(
                                nestedUnion.Modifiers,
                                AccessibilityUtilities.GetDefaultTypeAccessibility(parentType)));

                        InitializeTypeParameters(unionSymbol, nestedUnion.TypeParameterList, nestedUnion.ConstraintClauses);
                        nestedUnionSymbols[nestedUnion] = unionSymbol;
                        break;
                    }
            }
        }

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

                case OperatorDeclarationSyntax operatorDecl:
                    var operatorBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var boundOperatorBinder = operatorBinder.BindOperatorDeclaration(operatorDecl);
                    _binderCache[operatorDecl] = boundOperatorBinder;
                    break;

                case ConversionOperatorDeclarationSyntax conversionDecl:
                    var conversionBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var boundConversionBinder = conversionBinder.BindConversionOperatorDeclaration(conversionDecl);
                    _binderCache[conversionDecl] = boundConversionBinder;
                    break;

                case PropertyDeclarationSyntax propDecl:
                    var propMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var accessorBinders = propMemberBinder.BindPropertyDeclaration(propDecl);
                    _binderCache[propDecl] = propMemberBinder;
                    foreach (var kv in accessorBinders)
                        _binderCache[kv.Key] = kv.Value;
                    break;

                case EventDeclarationSyntax eventDecl:
                    var eventMemberBinder = new TypeMemberBinder(classBinder, (INamedTypeSymbol)classBinder.ContainingSymbol);
                    var eventAccessors = eventMemberBinder.BindEventDeclaration(eventDecl);
                    _binderCache[eventDecl] = eventMemberBinder;
                    foreach (var kv in eventAccessors)
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
                    var nestedBaseType = objectType;
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
                    var nestedSymbol = nestedClassSymbols[nestedClass];
                    if (nestedSymbol is SourceNamedTypeSymbol nestedRecordSymbol && nestedRecordSymbol.IsRecord)
                    {
                        if (Compilation.GetTypeByMetadataName("System.IEquatable`1") is INamedTypeSymbol equatableDefinition)
                        {
                            var equatableType = (INamedTypeSymbol)equatableDefinition.Construct(nestedSymbol);
                            if (nestedInterfaces.IsDefaultOrEmpty)
                            {
                                nestedInterfaces = ImmutableArray.Create(equatableType);
                            }
                            else if (!nestedInterfaces.Any(i => SymbolEqualityComparer.Default.Equals(i, equatableType)))
                            {
                                nestedInterfaces = nestedInterfaces.Add(equatableType);
                            }
                        }
                    }
                    if (nestedBaseType is not null &&
                        !SymbolEqualityComparer.Default.Equals(nestedSymbol.BaseType, nestedBaseType) &&
                        SymbolEqualityComparer.Default.Equals(nestedSymbol.BaseType, objectType))
                    {
                        nestedSymbol.SetBaseType(nestedBaseType);
                    }

                    if (!nestedInterfaces.IsDefaultOrEmpty)
                        nestedSymbol.SetInterfaces(MergeInterfaces(nestedSymbol.Interfaces, nestedInterfaces));

                    var nestedBinder = new ClassDeclarationBinder(classBinder, nestedSymbol, nestedClass);
                    nestedBinder.EnsureTypeParameterConstraintTypesResolved(nestedSymbol.TypeParameters);
                    _binderCache[nestedClass] = nestedBinder;
                    RegisterClassSymbol(nestedClass, nestedSymbol);
                    if (nestedClass.BaseList is not null && nestedBaseType!.IsStatic)
                    {
                        nestedBinder.Diagnostics.ReportStaticTypeCannotBeInherited(
                            nestedBaseType.Name,
                            nestedClass.BaseList.Types[0].GetLocation());
                    }
                    else if (nestedClass.BaseList is not null && nestedBaseType!.IsSealed)
                    {
                        nestedBinder.Diagnostics.ReportCannotInheritFromSealedType(
                            nestedBaseType.Name,
                            nestedClass.BaseList.Types[0].GetLocation());
                    }
                    RegisterClassMembers(nestedClass, nestedBinder);
                    nestedBinder.EnsureDefaultConstructor();
                    nestedClassBinders.Add((nestedClass, nestedBinder));
                    break;

                case UnionDeclarationSyntax nestedUnion:
                    {
                        var declaringSymbol = (ISymbol)classBinder.ContainingSymbol;
                        var namespaceSymbol = classBinder.CurrentNamespace?.AsSourceNamespace();
                        var unionSymbol = nestedUnionSymbols[nestedUnion];
                        var (unionBinder, resolvedSymbol) = RegisterUnionDeclaration(
                            nestedUnion,
                            classBinder,
                            declaringSymbol,
                            namespaceSymbol,
                            unionSymbol);
                        RegisterUnionCases(nestedUnion, unionBinder, resolvedSymbol);
                        break;
                    }

                case InterfaceDeclarationSyntax nestedInterface:
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

                    if (!parentInterfaces.IsDefaultOrEmpty)
                        nestedInterfaceSymbols[nestedInterface].SetInterfaces(
                            MergeInterfaces(nestedInterfaceSymbols[nestedInterface].Interfaces, parentInterfaces));
                    var nestedInterfaceSymbol = nestedInterfaceSymbols[nestedInterface];
                    var nestedInterfaceBinder = new InterfaceDeclarationBinder(classBinder, nestedInterfaceSymbol, nestedInterface);
                    nestedInterfaceBinder.EnsureTypeParameterConstraintTypesResolved(nestedInterfaceSymbol.TypeParameters);
                    _binderCache[nestedInterface] = nestedInterfaceBinder;
                    RegisterInterfaceMembers(nestedInterface, nestedInterfaceBinder);
                    break;

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumSymbol = nestedEnumSymbols[enumDecl];

                        var enumBinder = new EnumDeclarationBinder(classBinder, enumSymbol, enumDecl);
                        _binderCache[enumDecl] = enumBinder;

                        RegisterEnumMembers(enumDecl, enumBinder, enumSymbol, classBinder.CurrentNamespace!.AsSourceNamespace());
                        break;
                    }
            }
        }

        var checkedNestedClasses = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var (nestedSyntax, nestedBinder) in nestedClassBinders)
        {
            var nestedSymbol = (INamedTypeSymbol)nestedBinder.ContainingSymbol;
            if (!checkedNestedClasses.Add(nestedSymbol))
                continue;

            ReportMissingAbstractBaseMembers(nestedSymbol, nestedSyntax, nestedBinder.Diagnostics);
        }

        RegisterRecordValueMembers(classDecl, classBinder);
        classBinder.EnsureDefaultConstructor();

        static ImmutableArray<INamedTypeSymbol> MergeInterfaces(
            ImmutableArray<INamedTypeSymbol> existing,
            ImmutableArray<INamedTypeSymbol> additional)
        {
            if (existing.IsDefaultOrEmpty)
                return additional;

            var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
            var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
            foreach (var type in existing)
                if (seen.Add(type))
                    builder.Add(type);

            foreach (var type in additional)
                if (seen.Add(type))
                    builder.Add(type);

            return builder.ToImmutable();
        }
    }


    private void RegisterEnumMembers(
        EnumDeclarationSyntax enumDecl,
        EnumDeclarationBinder enumBinder,
        SourceNamedTypeSymbol enumSymbol,
        SourceNamespaceSymbol? containingNamespace)
    {
        // C#-like enum member semantics:
        // - If no initializer is present, the value is previous + 1 (first defaults to 0).
        // - If an initializer is present, it must be a constant expression.
        // - Previous enum members must be in scope while binding later initializers.
        var underlyingType = enumSymbol.EnumUnderlyingType;
        var nextValue = (object?)null;

        foreach (var enumMember in enumDecl.Members)
        {
            var memberValue = nextValue ?? GetDefaultEnumMemberValue(underlyingType);
            var equalsValue = enumMember.EqualsValue;
            if (equalsValue is not null)
            {
                var exprBinder = new BlockBinder(enumSymbol, enumBinder);
                var bound = exprBinder.BindExpression(equalsValue.Value);
                foreach (var diagnostic in exprBinder.Diagnostics.AsEnumerable())
                    enumBinder.Diagnostics.Report(diagnostic);

                CacheBoundNode(equalsValue.Value, bound);

                if (TryGetEnumMemberConstantValue(bound, out var constant) &&
                    TryConvertEnumMemberValue(underlyingType, constant, out memberValue))
                {
                    if (!TryIncrementEnumValue(memberValue, underlyingType, out nextValue))
                    {
                        enumBinder.Diagnostics.ReportEnumMemberValueCannotConvert(
                            enumMember.Identifier.ValueText,
                            underlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                            equalsValue.Value.GetLocation());
                        nextValue = null;
                    }
                }
                else
                {
                    var typeDisplay = underlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
                    if (!TryGetEnumMemberConstantValue(bound, out _))
                    {
                        enumBinder.Diagnostics.ReportEnumMemberValueMustBeConstant(
                            enumMember.Identifier.ValueText,
                            equalsValue.Value.GetLocation());
                    }
                    else
                    {
                        enumBinder.Diagnostics.ReportEnumMemberValueCannotConvert(
                            enumMember.Identifier.ValueText,
                            typeDisplay,
                            equalsValue.Value.GetLocation());
                    }

                    memberValue = nextValue ?? GetDefaultEnumMemberValue(underlyingType);
                    if (!TryIncrementEnumValue(memberValue, underlyingType, out nextValue))
                        nextValue = null;
                }
            }
            else
            {
                if (!TryIncrementEnumValue(memberValue, underlyingType, out nextValue))
                {
                    enumBinder.Diagnostics.ReportEnumMemberValueCannotConvert(
                        enumMember.Identifier.ValueText,
                        underlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                        enumMember.Identifier.GetLocation());
                    nextValue = null;
                }
            }

            var fieldSymbol = new SourceFieldSymbol(
                enumMember.Identifier.ValueText,
                enumSymbol,
                isStatic: true,
                isMutable: false,
                isConst: true,
                constantValue: memberValue,
                enumSymbol,
                enumSymbol,
                containingNamespace,
                new[] { enumMember.GetLocation() },
                new[] { enumMember.GetReference() },
                null,
                declaredAccessibility: Accessibility.Public);

            // Ensure later member initializers can resolve earlier members.
            if (!enumSymbol.GetMembers().Any(m => SymbolEqualityComparer.Default.Equals(m, fieldSymbol)))
                enumSymbol.AddMember(fieldSymbol);
        }
    }

    private ITypeSymbol ResolveEnumUnderlyingType(EnumDeclarationSyntax enumDecl, Binder binder)
    {
        var defaultType = Compilation.GetSpecialType(SpecialType.System_Int32);

        if (enumDecl.BaseList is not { } baseList || baseList.Types.Count == 0)
            return defaultType;

        if (baseList.Types.Count > 1)
        {
            for (var i = 1; i < baseList.Types.Count; i++)
            {
                binder.Diagnostics.ReportEnumUnderlyingTypeMustBeSingle(baseList.Types[i].GetLocation());
            }
        }

        var underlyingTypeSyntax = baseList.Types[0];
        var resolvedType = binder.ResolveType(underlyingTypeSyntax);

        if (resolvedType is null || resolvedType == Compilation.ErrorTypeSymbol)
            return defaultType;

        var normalizedType = UnwrapAliasType(resolvedType);
        if (normalizedType is NullableTypeSymbol || !IsValidEnumUnderlyingType(normalizedType))
        {
            binder.Diagnostics.ReportEnumUnderlyingTypeMustBeIntegral(
                resolvedType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                underlyingTypeSyntax.GetLocation());
            return defaultType;
        }

        return normalizedType;
    }

    private static bool IsValidEnumUnderlyingType(ITypeSymbol type)
    {
        return type.SpecialType is
            SpecialType.System_SByte or
            SpecialType.System_Byte or
            SpecialType.System_Int16 or
            SpecialType.System_UInt16 or
            SpecialType.System_Int32 or
            SpecialType.System_UInt32 or
            SpecialType.System_Int64 or
            SpecialType.System_UInt64 or
            SpecialType.System_Char;
    }

    private static ITypeSymbol UnwrapAliasType(ITypeSymbol type)
    {
        while (type is IAliasSymbol alias && alias.UnderlyingSymbol is ITypeSymbol underlying)
            type = underlying;

        return type;
    }

    private static object GetDefaultEnumMemberValue(ITypeSymbol underlyingType)
    {
        return underlyingType.SpecialType switch
        {
            SpecialType.System_SByte => (sbyte)0,
            SpecialType.System_Byte => (byte)0,
            SpecialType.System_Int16 => (short)0,
            SpecialType.System_UInt16 => (ushort)0,
            SpecialType.System_Int32 => 0,
            SpecialType.System_UInt32 => (uint)0,
            SpecialType.System_Int64 => 0L,
            SpecialType.System_UInt64 => 0UL,
            SpecialType.System_Char => '\0',
            _ => 0
        };
    }

    private static bool TryGetEnumMemberConstantValue(BoundExpression expression, out object? value)
    {
        switch (expression)
        {
            case BoundLiteralExpression literal when literal.Value is not null:
                value = literal.Value;
                return true;
            case BoundFieldAccess fieldAccess:
                {
                    var field = fieldAccess.Field;
                    if (field is not null && field.IsConst)
                    {
                        value = field.GetConstantValue();
                        return value is not null;
                    }

                    break;
                }
        }

        value = null;
        return false;
    }

    private bool TryConvertEnumMemberValue(ITypeSymbol underlyingType, object? value, out object? converted)
    {
        if (value is null)
        {
            converted = null;
            return false;
        }

        underlyingType = UnwrapAliasType(underlyingType);

        if (underlyingType.SpecialType == SpecialType.System_Char)
        {
            if (value is char ch)
            {
                converted = ch;
                return true;
            }

            if (ConstantValueEvaluator.TryConvert(
                    Compilation.GetSpecialType(SpecialType.System_UInt16),
                    value,
                    out var ushortValue) &&
                ushortValue is ushort u16)
            {
                converted = (char)u16;
                return true;
            }

            converted = null;
            return false;
        }

        return ConstantValueEvaluator.TryConvert(underlyingType, value, out converted);
    }

    private static bool TryIncrementEnumValue(object? value, ITypeSymbol underlyingType, out object? incremented)
    {
        switch (underlyingType.SpecialType)
        {
            case SpecialType.System_SByte when value is sbyte sb && sb < sbyte.MaxValue:
                incremented = (sbyte)(sb + 1);
                return true;
            case SpecialType.System_Byte when value is byte b && b < byte.MaxValue:
                incremented = (byte)(b + 1);
                return true;
            case SpecialType.System_Int16 when value is short s && s < short.MaxValue:
                incremented = (short)(s + 1);
                return true;
            case SpecialType.System_UInt16 when value is ushort us && us < ushort.MaxValue:
                incremented = (ushort)(us + 1);
                return true;
            case SpecialType.System_Int32 when value is int i && i < int.MaxValue:
                incremented = i + 1;
                return true;
            case SpecialType.System_UInt32 when value is uint ui && ui < uint.MaxValue:
                incremented = ui + 1;
                return true;
            case SpecialType.System_Int64 when value is long l && l < long.MaxValue:
                incremented = l + 1;
                return true;
            case SpecialType.System_UInt64 when value is ulong ul && ul < ulong.MaxValue:
                incremented = ul + 1;
                return true;
            case SpecialType.System_Char when value is char ch && ch < char.MaxValue:
                incremented = (char)(ch + 1);
                return true;
        }

        incremented = null;
        return false;
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
                case OperatorDeclarationSyntax operatorDecl:
                    {
                        var memberBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var operatorBinder = memberBinder.BindOperatorDeclaration(operatorDecl);
                        _binderCache[operatorDecl] = operatorBinder;
                        break;
                    }
                case ConversionOperatorDeclarationSyntax conversionDecl:
                    {
                        var memberBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var conversionBinder = memberBinder.BindConversionOperatorDeclaration(conversionDecl);
                        _binderCache[conversionDecl] = conversionBinder;
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
                case EventDeclarationSyntax eventDecl:
                    {
                        var eventBinder = new TypeMemberBinder(interfaceBinder, (INamedTypeSymbol)interfaceBinder.ContainingSymbol);
                        var accessorBinders = eventBinder.BindEventDeclaration(eventDecl);
                        _binderCache[eventDecl] = eventBinder;
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
        if (extensionBinder.ContainingSymbol is SourceNamedTypeSymbol extensionSymbol)
        {
            var receiverType = extensionBinder.ResolveType(extensionDecl.ReceiverType);
            extensionSymbol.SetExtensionReceiverType(receiverType);
        }

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

                case OperatorDeclarationSyntax operatorDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var operatorBinder = memberBinder.BindOperatorDeclaration(operatorDecl);
                        _binderCache[operatorDecl] = operatorBinder;
                        break;
                    }
                case ConversionOperatorDeclarationSyntax conversionDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var conversionBinder = memberBinder.BindConversionOperatorDeclaration(conversionDecl);
                        _binderCache[conversionDecl] = conversionBinder;
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
                case EventDeclarationSyntax eventDecl:
                    {
                        var memberBinder = new TypeMemberBinder(extensionBinder, (INamedTypeSymbol)extensionBinder.ContainingSymbol, extensionDecl.ReceiverType);
                        var accessorBinders = memberBinder.BindEventDeclaration(eventDecl);
                        _binderCache[eventDecl] = memberBinder;
                        foreach (var kv in accessorBinders)
                            _binderCache[kv.Key] = kv.Value;
                        break;
                    }
            }
        }
    }

    private static void InitializeTypeParameters(
        SourceNamedTypeSymbol typeSymbol,
        TypeParameterListSyntax? typeParameterList,
        SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses)
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            return;

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
        var ordinal = 0;

        // Optional: pre-index clauses by name for fast lookup and duplicate detection
        var clausesByName = new Dictionary<string, List<TypeParameterConstraintClauseSyntax>>(StringComparer.Ordinal);
        foreach (var clause in constraintClauses)
        {
            var name = clause.TypeParameter.Identifier.ValueText;
            if (!clausesByName.TryGetValue(name, out var list))
                clausesByName[name] = list = new List<TypeParameterConstraintClauseSyntax>();
            list.Add(clause);
        }

        foreach (var parameter in typeParameterList.Parameters)
        {
            var name = parameter.Identifier.ValueText;

            // 1) inline
            var (inlineKind, inlineRefs) = TypeParameterConstraintAnalyzer.AnalyzeInline(parameter);

            // 2) where clauses for this parameter
            TypeParameterConstraintKind clauseKind = TypeParameterConstraintKind.None;
            var clauseRefsBuilder = ImmutableArray.CreateBuilder<SyntaxReference>();

            if (clausesByName.TryGetValue(name, out var matchingClauses))
            {
                // If you want “at most one where per type parameter”, enforce here.
                // Otherwise, allow multiple and merge.
                foreach (var clause in matchingClauses)
                {
                    var (k, refs) = TypeParameterConstraintAnalyzer.AnalyzeClause(clause);
                    clauseKind |= k;
                    clauseRefsBuilder.AddRange(refs);
                }
            }

            var mergedKind = inlineKind | clauseKind;
            var mergedRefs = inlineRefs.AddRange(clauseRefsBuilder.ToImmutable());

            var variance = GetDeclaredVariance(parameter);

            var typeParameter = new SourceTypeParameterSymbol(
                name,
                typeSymbol,
                typeSymbol,
                typeSymbol.ContainingNamespace,
                [parameter.GetLocation()],
                [parameter.GetReference()],
                ordinal++,
                mergedKind,
                mergedRefs,
                variance);

            builder.Add(typeParameter);
        }

        // Optional: report where-clauses that reference unknown type parameters
        // (you likely want diagnostics for this — but that needs a DiagnosticBag)
        // You can do it in the binder or in the place where you call this helper.

        typeSymbol.SetTypeParameters(builder.MoveToImmutable());
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
        var isRecord = classSymbol.IsRecord;

        if (classSymbol.IsStatic)
        {
            classBinder.Diagnostics.ReportStaticClassCannotContainInstanceMember(
                classSymbol.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat),
                "init",
                classDecl.ParameterList!.GetLocation());
            return;
        }

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
        var recordProperties = isRecord
            ? ImmutableArray.CreateBuilder<SourcePropertySymbol>()
            : null;

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
                if (isRecord)
                {
                    var propertySymbol = CreateRecordPropertyFromPrimaryConstructor(
                        classSymbol,
                        parameterSymbol,
                        parameterSyntax,
                        parameterType,
                        namespaceSymbol,
                        classBinder);

                    if (propertySymbol is not null)
                        recordProperties?.Add(propertySymbol);
                }
                else
                {
                    _ = new SourceFieldSymbol(
                        parameterSyntax.Identifier.ValueText,
                        parameterType,
                        isStatic: false,
                        isMutable: parameterSymbol.IsMutable,
                        isConst: false,
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
        }

        constructorSymbol.SetParameters(parameters);

        if (recordProperties is not null)
            classSymbol.SetRecordProperties(recordProperties.ToImmutable());
    }

    private SourcePropertySymbol? CreateRecordPropertyFromPrimaryConstructor(
        SourceNamedTypeSymbol classSymbol,
        SourceParameterSymbol parameterSymbol,
        ParameterSyntax parameterSyntax,
        ITypeSymbol parameterType,
        SourceNamespaceSymbol? namespaceSymbol,
        ClassDeclarationBinder classBinder)
    {
        var propertyName = parameterSyntax.Identifier.ValueText;
        if (classSymbol.IsMemberDefined(propertyName, out _))
        {
            classBinder.Diagnostics.ReportTypeAlreadyDefinesMember(
                classSymbol.Name,
                propertyName,
                parameterSyntax.Identifier.GetLocation());
            return null;
        }

        var location = parameterSyntax.GetLocation();
        var references = Array.Empty<SyntaxReference>();

        var propertySymbol = new SourcePropertySymbol(
            propertyName,
            parameterType,
            classSymbol,
            classSymbol,
            namespaceSymbol,
            [location],
            references,
            isStatic: false,
            declaredAccessibility: Accessibility.Public);

        var backingField = new SourceFieldSymbol(
            $"<{propertySymbol.Name}>k__BackingField",
            parameterType,
            isStatic: false,
            isMutable: parameterSymbol.IsMutable,
            isConst: false,
            constantValue: null,
            classSymbol,
            classSymbol,
            namespaceSymbol,
            [location],
            references,
            new BoundParameterAccess(parameterSymbol),
            declaredAccessibility: Accessibility.Private);

        propertySymbol.SetBackingField(backingField);

        var getMethod = new SourceMethodSymbol(
            $"get_{propertySymbol.Name}",
            parameterType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            propertySymbol,
            classSymbol,
            namespaceSymbol,
            [location],
            references,
            isStatic: false,
            methodKind: MethodKind.PropertyGet,
            declaredAccessibility: Accessibility.Public);

        SourceMethodSymbol? setMethod = null;
        if (parameterSymbol.IsMutable)
        {
            setMethod = new SourceMethodSymbol(
                $"set_{propertySymbol.Name}",
                Compilation.GetSpecialType(SpecialType.System_Unit),
                ImmutableArray<SourceParameterSymbol>.Empty,
                propertySymbol,
                classSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.PropertySet,
                declaredAccessibility: Accessibility.Public);

            var valueParameter = new SourceParameterSymbol(
                "value",
                parameterType,
                setMethod,
                classSymbol,
                namespaceSymbol,
                [location],
                references);
            setMethod.SetParameters(ImmutableArray.Create(valueParameter));
        }

        propertySymbol.SetAccessors(getMethod, setMethod);
        return propertySymbol;
    }

    private void RegisterRecordValueMembers(ClassDeclarationSyntax classDecl, ClassDeclarationBinder classBinder)
    {
        if (classBinder.ContainingSymbol is not SourceNamedTypeSymbol recordSymbol || !recordSymbol.IsRecord)
            return;

        var boolType = Compilation.GetSpecialType(SpecialType.System_Boolean);
        var intType = Compilation.GetSpecialType(SpecialType.System_Int32);
        var objectType = Compilation.GetSpecialType(SpecialType.System_Object);
        var unitType = Compilation.GetSpecialType(SpecialType.System_Unit);
        var namespaceSymbol = classBinder.CurrentNamespace!.AsSourceNamespace();
        var location = classDecl.GetLocation();
        var references = Array.Empty<SyntaxReference>();

        if (objectType is null || boolType is null || intType is null || unitType is null)
            return;

        var equalsObject = objectType.GetMembers(nameof(object.Equals))
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 1 && m.Parameters[0].Type.SpecialType == SpecialType.System_Object);

        var getHashCode = objectType.GetMembers(nameof(object.GetHashCode))
            .OfType<IMethodSymbol>()
            .FirstOrDefault(m => m.Parameters.Length == 0);

        if (equalsObject is null || getHashCode is null)
            return;

        if (!HasMethod(recordSymbol, "Equals", MethodKind.Ordinary, recordSymbol))
        {
            var equalsTyped = new SourceMethodSymbol(
                "Equals",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                declaredAccessibility: Accessibility.Public);

            var otherParameter = new SourceParameterSymbol(
                "other",
                recordSymbol,
                equalsTyped,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            equalsTyped.SetParameters(ImmutableArray.Create(otherParameter));
        }

        if (!HasMethod(recordSymbol, "Equals", MethodKind.Ordinary, objectType))
        {
            var equalsObj = new SourceMethodSymbol(
                "Equals",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                isOverride: true,
                declaredAccessibility: Accessibility.Public);

            equalsObj.SetOverriddenMethod(equalsObject);

            var otherParameter = new SourceParameterSymbol(
                "obj",
                objectType,
                equalsObj,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            equalsObj.SetParameters(ImmutableArray.Create(otherParameter));
        }

        if (!HasMethod(recordSymbol, "GetHashCode", MethodKind.Ordinary))
        {
            var hashMethod = new SourceMethodSymbol(
                "GetHashCode",
                intType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                isOverride: true,
                declaredAccessibility: Accessibility.Public);

            hashMethod.SetOverriddenMethod(getHashCode);
        }

        if (!HasMethod(recordSymbol, "op_Equality", MethodKind.UserDefinedOperator, recordSymbol, recordSymbol))
        {
            var equalsOperator = new SourceMethodSymbol(
                "op_Equality",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: true,
                methodKind: MethodKind.UserDefinedOperator,
                declaredAccessibility: Accessibility.Public);

            var leftParameter = new SourceParameterSymbol(
                "left",
                recordSymbol,
                equalsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            var rightParameter = new SourceParameterSymbol(
                "right",
                recordSymbol,
                equalsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            equalsOperator.SetParameters(ImmutableArray.Create(leftParameter, rightParameter));
        }

        if (!HasMethod(recordSymbol, "op_Inequality", MethodKind.UserDefinedOperator, recordSymbol, recordSymbol))
        {
            var notEqualsOperator = new SourceMethodSymbol(
                "op_Inequality",
                boolType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: true,
                methodKind: MethodKind.UserDefinedOperator,
                declaredAccessibility: Accessibility.Public);

            var leftParameter = new SourceParameterSymbol(
                "left",
                recordSymbol,
                notEqualsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            var rightParameter = new SourceParameterSymbol(
                "right",
                recordSymbol,
                notEqualsOperator,
                recordSymbol,
                namespaceSymbol,
                [location],
                references);
            notEqualsOperator.SetParameters(ImmutableArray.Create(leftParameter, rightParameter));
        }

        if (!HasDeconstruct(recordSymbol))
        {
            var deconstructMethod = new SourceMethodSymbol(
                "Deconstruct",
                unitType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                recordSymbol,
                recordSymbol,
                namespaceSymbol,
                [location],
                references,
                isStatic: false,
                methodKind: MethodKind.Ordinary,
                declaredAccessibility: Accessibility.Public);

            var parameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>(recordSymbol.RecordProperties.Length);
            foreach (var property in recordSymbol.RecordProperties)
            {
                var parameter = new SourceParameterSymbol(
                    property.Name,
                    property.Type,
                    deconstructMethod,
                    recordSymbol,
                    namespaceSymbol,
                    [location],
                    references,
                    refKind: RefKind.Out);

                parameters.Add(parameter);
            }

            deconstructMethod.SetParameters(parameters.ToImmutable());
        }
    }

    private static bool HasMethod(
        SourceNamedTypeSymbol typeSymbol,
        string name,
        MethodKind methodKind,
        params ITypeSymbol[] parameters)
    {
        foreach (var method in typeSymbol.GetMembers(name).OfType<IMethodSymbol>())
        {
            if (method.MethodKind != methodKind)
                continue;

            if (method.Parameters.Length != parameters.Length)
                continue;

            var matches = true;
            for (var i = 0; i < parameters.Length; i++)
            {
                if (!SymbolEqualityComparer.Default.Equals(method.Parameters[i].Type, parameters[i]))
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
                return true;
        }

        return false;
    }

    private static bool HasDeconstruct(SourceNamedTypeSymbol typeSymbol)
    {
        foreach (var method in typeSymbol.GetMembers("Deconstruct").OfType<IMethodSymbol>())
        {
            if (method.MethodKind != MethodKind.Ordinary || method.IsStatic)
                continue;

            if (method.ReturnType.SpecialType != SpecialType.System_Unit)
                continue;

            if (method.Parameters.Any(parameter => parameter.RefKind != RefKind.Out))
                continue;

            if (method.Parameters.Length != typeSymbol.RecordProperties.Length)
                continue;

            return true;
        }

        return false;
    }

    private void ReportMissingAbstractBaseMembers(
        INamedTypeSymbol typeSymbol,
        ClassDeclarationSyntax declaration,
        DiagnosticBag diagnostics)
    {
        if (typeSymbol.IsAbstract)
            return;

        if (typeSymbol.BaseType is not INamedTypeSymbol baseType ||
            baseType.TypeKind != TypeKind.Class)
        {
            return;
        }

        foreach (var abstractMember in baseType.GetMembers().OfType<IMethodSymbol>())
        {
            if (!abstractMember.IsAbstract || abstractMember.IsStatic)
                continue;

            if (abstractMember.MethodKind is MethodKind.Constructor or MethodKind.StaticConstructor)
                continue;

            if (ImplementsAbstractMember(typeSymbol, abstractMember))
                continue;

            var memberDisplay = GetAbstractMemberDisplay(abstractMember);
            var baseTypeDisplay = baseType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);

            diagnostics.ReportTypeDoesNotImplementAbstractMember(
                typeSymbol.Name,
                memberDisplay,
                baseTypeDisplay,
                declaration.Identifier.GetLocation());
        }
    }

    private static bool ImplementsAbstractMember(INamedTypeSymbol typeSymbol, IMethodSymbol abstractMember)
    {
        foreach (var candidate in typeSymbol.GetMembers(abstractMember.Name).OfType<IMethodSymbol>())
        {
            if (!candidate.IsOverride)
                continue;

            if (candidate is SourceMethodSymbol sourceMethod &&
                sourceMethod.OverriddenMethod is not null &&
                SymbolEqualityComparer.Default.Equals(sourceMethod.OverriddenMethod, abstractMember))
            {
                return true;
            }
        }

        return false;
    }

    private static string GetAbstractMemberDisplay(IMethodSymbol abstractMember)
    {
        if (abstractMember.AssociatedSymbol is IPropertySymbol property)
        {
            var propertyFormat = SymbolDisplayFormat.MinimallyQualifiedFormat.WithMemberOptions(SymbolDisplayMemberOptions.None);
            return property.ToDisplayString(propertyFormat);
        }

        var methodFormat = SymbolDisplayFormat.MinimallyQualifiedFormat.WithMemberOptions(SymbolDisplayMemberOptions.IncludeParameters);
        return abstractMember.ToDisplayString(methodFormat);
    }

    internal BoundNode? TryGetCachedBoundNode(SyntaxNode node)
        => _boundNodeCache.TryGetValue(node, out var bound) ? bound : null;

    internal void CacheBoundNode(SyntaxNode node, BoundNode bound)
    {
        _boundNodeCache[node] = bound;
        _syntaxCache[bound] = node;
    }

    internal void RemoveCachedBoundNode(SyntaxNode node)
    {
        if (_boundNodeCache.TryGetValue(node, out var bound))
            _syntaxCache.Remove(bound);

        _boundNodeCache.Remove(node);
    }

    internal SyntaxNode? GetSyntax(BoundNode node)
        => _syntaxCache.TryGetValue(node, out var syntax) ? syntax : null;

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
