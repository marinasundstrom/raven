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
                if (child is GlobalStatementSyntax)
                    continue;

                var childBinder = GetBinder(child, currentBinder);

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

        var binder = GetBinder(node);
        var info = binder.BindSymbol(node);
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
        var binder = GetBinder(expr);

        var boundExpr = binder.BindExpression(expr);

        if (boundExpr is null || boundExpr is BoundErrorExpression)
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
            targetNamespace = Compilation.GetNamespaceSymbol(fileScopedNamespace.Name.ToString())
                             ?? throw new Exception("Namespace not found");

            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace);
            parentBinder = namespaceBinder;
            _binderCache[fileScopedNamespace] = namespaceBinder;
        }
        else
        {
            targetNamespace = Compilation.SourceGlobalNamespace;
            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace);
            parentBinder = namespaceBinder;
        }

        // Step 2: Handle imports
        var namespaceImports = new List<INamespaceSymbol>();
        var typeImports = new List<ITypeSymbol>();
        var aliases = new Dictionary<string, ITypeSymbol>();

        foreach (var import in cu.DescendantNodes().OfType<ImportDirectiveSyntax>())
        {
            var name = import.Name.ToString();

            if (IsWildcard(import.Name, out var nsName))
            {
                var nsImport = ResolveNamespace(targetNamespace, nsName.ToString());
                if (nsImport != null)
                    namespaceImports.Add(nsImport);
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
        }

        foreach (var alias in cu.DescendantNodes().OfType<AliasDirectiveSyntax>())
        {
            ITypeSymbol? typeSymbol = HasTypeArguments(alias.Name)
                ? ResolveGenericType(targetNamespace, alias.Name)
                : ResolveType(targetNamespace, alias.Name.ToString());

            if (typeSymbol != null)
            {
                aliases[alias.Identifier.Text] = typeSymbol;
            }
        }

        var importBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);
        parentBinder = importBinder;

        var compilationUnitBinder = new CompilationUnitBinder(parentBinder, this);
        RegisterNamespaceMembers(cu, compilationUnitBinder, targetNamespace);

        CreateTopLevelBinder(cu, targetNamespace, importBinder);

        _binderCache[cu] = namespaceBinder;

        return namespaceBinder;

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
                    .Select(a => namespaceBinder.ResolveType(a.Type))
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
                    .Select(a => namespaceBinder.ResolveType(a.Type))
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
                    .Select(a => namespaceBinder.ResolveType(a.Type))
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

        var globals = cu.DescendantNodes().OfType<GlobalStatementSyntax>().ToList();
        topLevelBinder.BindGlobalStatements(globals);

        foreach (var stmt in globals)
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
                        var nsSymbol = Compilation.GetNamespaceSymbol(nsDecl.Name.ToString())
                                        ?? throw new Exception($"Namespace not found: {nsDecl.Name}");

                        var nsBinder = new NamespaceBinder(parentBinder, nsSymbol);
                        _binderCache[nsDecl] = nsBinder;

                        RegisterNamespaceMembers(nsDecl, nsBinder, nsSymbol);
                        break;
                    }

                case ClassDeclarationSyntax classDecl:
                    {
                        var classSymbol = new SourceNamedTypeSymbol(
                            classDecl.Identifier.Text,
                            Compilation.GetTypeByMetadataName("System.Object"),
                            TypeKind.Class,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            [classDecl.GetLocation()],
                            [classDecl.GetReference()]
                        );

                        var classBinder = new ClassDeclarationBinder(parentBinder, classSymbol, classDecl);
                        _binderCache[classDecl] = classBinder;
                        RegisterClassSymbol(classDecl, classSymbol);
                        RegisterClassMembers(classDecl, classBinder);
                        break;
                    }

                case EnumDeclarationSyntax enumDecl:
                    {
                        var enumSymbol = new SourceNamedTypeSymbol(
                            enumDecl.Identifier.Text,
                            Compilation.GetTypeByMetadataName("System.Enum"),
                            TypeKind.Enum,
                            parentNamespace.AsSourceNamespace(),
                            null,
                            parentNamespace.AsSourceNamespace(),
                            [enumDecl.GetLocation()],
                            [enumDecl.GetReference()]
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
                                [enumMember.GetReference()]
                            );
                        }

                        break;
                    }
            }
        }
    }

    private void RegisterClassMembers(ClassDeclarationSyntax classDecl, ClassDeclarationBinder classBinder)
    {
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
                    var nestedSymbol = new SourceNamedTypeSymbol(
                        nestedClass.Identifier.Text,
                        Compilation.GetTypeByMetadataName("System.Object"),
                        TypeKind.Class,
                        parentType,
                        parentType,
                        classBinder.CurrentNamespace!.AsSourceNamespace(),
                        [nestedClass.GetLocation()],
                        [nestedClass.GetReference()]
                    );

                    var nestedBinder = new ClassDeclarationBinder(classBinder, nestedSymbol, nestedClass);
                    _binderCache[nestedClass] = nestedBinder;
                    RegisterClassSymbol(nestedClass, nestedSymbol);
                    RegisterClassMembers(nestedClass, nestedBinder);
                    nestedBinder.EnsureDefaultConstructor();
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
                        [enumDecl.GetReference()]
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
                            [enumMember.GetReference()]
                        );
                    }
                    break;
            }
        }

        classBinder.EnsureDefaultConstructor();
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
