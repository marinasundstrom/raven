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
                    namespaceImports.Add(nsImport);
                else
                    namespaceBinder.Diagnostics.ReportInvalidImportTarget(nsName.GetLocation());
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

        foreach (var alias in cu.Aliases)
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
                var aliasSymbols = symbols
                    .Select(s => AliasSymbolFactory.Create(alias.Identifier.Text, s))
                    .ToArray();
                aliases[alias.Identifier.Text] = aliasSymbols;
            }
            else
            {
                namespaceBinder.Diagnostics.ReportInvalidAliasType(alias.Target.GetLocation());
            }
        }

        var importBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);

        foreach (var diagnostic in namespaceBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        parentBinder = importBinder;

        var compilationUnitBinder = new CompilationUnitBinder(parentBinder, this);
        RegisterNamespaceMembers(cu, compilationUnitBinder, targetNamespace);

        CreateTopLevelBinder(cu, targetNamespace, importBinder);

        _binderCache[cu] = importBinder;
        if (fileScopedNamespace != null)
            _binderCache[fileScopedNamespace] = importBinder;

        return importBinder;

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
                var baseName = $"{g.Identifier.Text}`{g.TypeArgumentList.Arguments.Count}";
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
                var baseName = $"{leftName}.{gen.Identifier.Text}`{gen.TypeArgumentList.Arguments.Count}";
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
                var baseName = $"{g.Identifier.Text}`{g.TypeArgumentList.Arguments.SeparatorCount + 1}";
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
                var baseName = $"{leftName}.{gen.Identifier.Text}`{gen.TypeArgumentList.Arguments.SeparatorCount + 1}";
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
                IdentifierNameSyntax id => id.Identifier.Text,
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
        var programClass = new SynthesizedProgramClassSymbol(Compilation, namespaceSymbol.AsSourceNamespace(), [cu.GetLocation()], [cu.GetReference()]);
        var mainMethod = new SynthesizedMainMethodSymbol(programClass, [cu.GetLocation()], [cu.GetReference()]);
        var topLevelBinder = new TopLevelBinder(parentBinder, this, mainMethod);

        var fileScopedNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();

        var allGlobals = cu.DescendantNodes().OfType<GlobalStatementSyntax>().ToList();
        var bindableGlobals = new List<GlobalStatementSyntax>();

        foreach (var global in allGlobals)
        {
            if (global.Parent is CompilationUnitSyntax || global.Parent is FileScopedNamespaceDeclarationSyntax)
                bindableGlobals.Add(global);
        }

        void CheckOrder(SyntaxList<MemberDeclarationSyntax> members)
        {
            var seenNonGlobal = false;
            foreach (var member in members)
            {
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

        topLevelBinder.BindGlobalStatements(bindableGlobals);

        foreach (var stmt in bindableGlobals)
            _binderCache[stmt] = topLevelBinder;

        return topLevelBinder;
    }

    private void RegisterNamespaceMembers(SyntaxNode containerNode, Binder parentBinder, INamespaceSymbol parentNamespace)
    {
        foreach (var member in containerNode.ChildNodes())
        {
            switch (member)
            {
                case BaseNamespaceDeclarationSyntax nsDecl:
                    {
                        var nsSymbol = Compilation.GetOrCreateNamespaceSymbol(nsDecl.Name.ToString());

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
                        var typeAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            classDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));

                        var classSymbol = new SourceNamedTypeSymbol(
                            classDecl.Identifier.Text,
                            baseTypeSymbol!,
                            TypeKind.Class,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            [classDecl.GetLocation()],
                            [classDecl.GetReference()],
                            isSealed,
                            isAbstract,
                            declaredAccessibility: typeAccessibility);

                        InitializeTypeParameters(classSymbol, classDecl.TypeParameterList);

                        if (!interfaceList.IsDefaultOrEmpty)
                            classSymbol.SetInterfaces(interfaceList);

                        var classBinder = new ClassDeclarationBinder(parentBinder, classSymbol, classDecl);
                        classBinder.EnsureTypeParameterConstraintTypesResolved(classSymbol.TypeParameters);
                        _binderCache[classDecl] = classBinder;
                        RegisterClassSymbol(classDecl, classSymbol);
                        if (classDecl.BaseList is not null && baseTypeSymbol!.IsSealed)
                            classBinder.Diagnostics.ReportCannotInheritFromSealedType(baseTypeSymbol.Name, classDecl.BaseList.Types[0].GetLocation());
                        RegisterClassMembers(classDecl, classBinder);
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

                        var interfaceSymbol = new SourceNamedTypeSymbol(
                            interfaceDecl.Identifier.Text,
                            Compilation.GetTypeByMetadataName("System.Object")!,
                            TypeKind.Interface,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            [interfaceDecl.GetLocation()],
                            [interfaceDecl.GetReference()],
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
                        RegisterInterfaceMembers(interfaceDecl, interfaceBinder);
                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumAccessibility = AccessibilityUtilities.DetermineAccessibility(
                            enumDecl.Modifiers,
                            AccessibilityUtilities.GetDefaultTypeAccessibility(parentNamespace.AsSourceNamespace()));
                        var enumSymbol = new SourceNamedTypeSymbol(
                            enumDecl.Identifier.Text,
                            Compilation.GetTypeByMetadataName("System.Enum"),
                            TypeKind.Enum,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            [enumDecl.GetLocation()],
                            [enumDecl.GetReference()],
                            true,
                            declaredAccessibility: enumAccessibility
                        );

                        var enumBinder = new EnumDeclarationBinder(parentBinder, enumSymbol, enumDecl);
                        _binderCache[enumDecl] = enumBinder;

                        int value = 0;
                        foreach (var enumMember in enumDecl.Members)
                        {
                            _ = new SourceFieldSymbol(
                                enumMember.Identifier.Text,
                                enumSymbol,
                                isStatic: true,
                                isLiteral: true,
                                constantValue: value++,
                                enumSymbol,
                                enumSymbol,
                                parentNamespace.AsSourceNamespace(),
                                [enumMember.GetLocation()],
                                [enumMember.GetReference()],
                                null,
                                declaredAccessibility: Accessibility.Public
                            );
                        }

                        break;
                    }
            }
        }
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
                    var nestedAccessibility = AccessibilityUtilities.DetermineAccessibility(
                        nestedClass.Modifiers,
                        AccessibilityUtilities.GetDefaultTypeAccessibility(parentType));
                    var nestedSymbol = new SourceNamedTypeSymbol(
                        nestedClass.Identifier.Text,
                        nestedBaseType!,
                        TypeKind.Class,
                        parentType,
                        parentType,
                        classBinder.CurrentNamespace!.AsSourceNamespace(),
                        [nestedClass.GetLocation()],
                        [nestedClass.GetReference()],
                        nestedSealed,
                        nestedAbstract,
                        declaredAccessibility: nestedAccessibility
                    );

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
                        nestedInterface.Identifier.Text,
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
                        enumDecl.Identifier.Text,
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
                            enumMember.Identifier.Text,
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
                            nestedInterface.Identifier.Text,
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

    private static void InitializeTypeParameters(SourceNamedTypeSymbol typeSymbol, TypeParameterListSyntax? typeParameterList)
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            return;

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
        var ordinal = 0;

        foreach (var parameter in typeParameterList.Parameters)
        {
            var (constraintKind, constraintTypeReferences) = AnalyzeTypeParameterConstraints(parameter);

            var typeParameter = new SourceTypeParameterSymbol(
                parameter.Identifier.Text,
                typeSymbol,
                typeSymbol,
                typeSymbol.ContainingNamespace,
                [parameter.GetLocation()],
                [parameter.GetReference()],
                ordinal++,
                constraintKind,
                constraintTypeReferences);

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

        foreach (var parameterSyntax in classDecl.ParameterList!.Parameters)
        {
            var refKind = RefKind.None;
            if (parameterSyntax.Modifiers.Any(m => m.Kind == SyntaxKind.OutKeyword))
                refKind = RefKind.Out;
            else if (parameterSyntax.Modifiers.Any(m => m.Kind == SyntaxKind.InKeyword))
                refKind = RefKind.In;
            else if (parameterSyntax.Modifiers.Any(m => m.Kind == SyntaxKind.RefKeyword))
                refKind = RefKind.Ref;

            var typeSyntax = parameterSyntax.TypeAnnotation?.Type;
            if (typeSyntax is ByRefTypeSyntax byRefType)
                typeSyntax = byRefType.ElementType;

            var parameterType = typeSyntax is null
                ? Compilation.ErrorTypeSymbol
                : classBinder.ResolveType(typeSyntax);

            var parameterSymbol = new SourceParameterSymbol(
                parameterSyntax.Identifier.Text,
                parameterType,
                constructorSymbol,
                classSymbol,
                namespaceSymbol,
                [parameterSyntax.GetLocation()],
                [parameterSyntax.GetReference()],
                refKind);

            parameters.Add(parameterSymbol);

            if (refKind == RefKind.None)
            {
                _ = new SourceFieldSymbol(
                    parameterSyntax.Identifier.Text,
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

    private readonly Dictionary<ClassDeclarationSyntax, SourceNamedTypeSymbol> _classSymbols = new();

    internal void RegisterClassSymbol(ClassDeclarationSyntax node, SourceNamedTypeSymbol symbol)
        => _classSymbols[node] = symbol;

    internal SourceNamedTypeSymbol GetClassSymbol(ClassDeclarationSyntax node)
        => _classSymbols[node];
}
