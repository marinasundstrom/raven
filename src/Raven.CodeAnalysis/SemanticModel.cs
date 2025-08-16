using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly Dictionary<SyntaxNode, Binder> _binderCache = new();
    private readonly Dictionary<SyntaxNode, SymbolInfo> _symbolMappings = new();
    private readonly Dictionary<SyntaxNode, BoundNode> _boundNodeCache = new();

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
        EnsureDiagnosticsCollected();

        return _binderCache.Values
             .SelectMany(b => b.Diagnostics.AsEnumerable())
             .ToImmutableArray();
    }

    private void EnsureDiagnosticsCollected()
    {
        var root = SyntaxTree.GetRoot();

        var topLevelBinder = GetBinder(root) as TopLevelBinder;

        foreach (var globalStmt in root.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            topLevelBinder.BindGlobalStatement(globalStmt);
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
        var info = binder.BindReferencedSymbol(node);
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
            var topLevelBinder = CreateTopLevelBinder(cu, Compilation.GlobalBinder);
            _binderCache[cu] = topLevelBinder;
            return topLevelBinder;
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

    private Binder CreateTopLevelBinder(CompilationUnitSyntax cu, Binder parentBinder)
    {
        // Step 1: Resolve namespace
        var fileScopedNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();
        INamespaceSymbol targetNamespace;
        NamespaceBinder namespaceBinder;

        if (fileScopedNamespace != null)
        {
            targetNamespace = Compilation.GetNamespaceSymbol(fileScopedNamespace.Name.ToString())
                             ?? throw new Exception("Namespace not found");

            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace, Compilation, this);
            parentBinder = namespaceBinder;
            _binderCache[fileScopedNamespace] = namespaceBinder;
        }
        else
        {
            targetNamespace = Compilation.SourceGlobalNamespace;
            namespaceBinder = new NamespaceBinder(parentBinder, targetNamespace, Compilation, this);
            parentBinder = namespaceBinder;
        }

        // Step 2: Handle imports
        var imports = cu.DescendantNodes().OfType<ImportDirectiveSyntax>()
            .Select(i => Compilation.GetNamespaceSymbol(i.NamespaceOrType.ToString()))
            .OfType<INamespaceSymbol>()
            .ToList();

        var importBinder = new ImportBinder(namespaceBinder, imports);
        parentBinder = importBinder;

        // Step 3: Create synthesized Main
        var programClass = new SynthesizedProgramClassSymbol(Compilation, targetNamespace.AsSourceNamespace(), [cu.GetLocation()], [cu.GetReference()]);
        var mainMethod = new SynthesizedMainMethodSymbol(programClass, [cu.GetLocation()], [cu.GetReference()]);
        var topLevelBinder = new TopLevelBinder(importBinder, this, mainMethod);

        _binderCache[cu] = topLevelBinder;

        RegisterNamespaceMembers(cu, parentBinder, targetNamespace);

        // Step 5: Register and bind global statements
        foreach (var stmt in cu.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            if (stmt.Statement is LocalFunctionStatementSyntax localFunc)
            {
                var binder = GetBinder(localFunc, topLevelBinder);
                if (binder is LocalFunctionBinder lfBinder)
                    topLevelBinder.DeclareLocalFunction(lfBinder.GetMethodSymbol());
            }
        }

        foreach (var stmt in cu.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            topLevelBinder.BindGlobalStatement(stmt);
        }

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

                        var nsBinder = new NamespaceBinder(parentBinder, nsSymbol, Compilation);
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

                        var classBinder = new TypeDeclarationBinder(parentBinder, classSymbol);
                        _binderCache[classDecl] = classBinder;
                        RegisterClassSymbol(classDecl, classSymbol);
                        RegisterClassMembers(classDecl, classSymbol, classBinder, parentNamespace);
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

    private void RegisterClassMembers(ClassDeclarationSyntax classDecl, SourceNamedTypeSymbol classSymbol, TypeDeclarationBinder classBinder, INamespaceSymbol parentNamespace)
    {
        foreach (var member in classDecl.Members)
        {
            switch (member)
            {
                case FieldDeclarationSyntax fieldDecl:
                    {
                        foreach (var decl in fieldDecl.Declaration.Declarators)
                        {
                            var fieldType = decl.TypeAnnotation is null
                                ? Compilation.GetSpecialType(SpecialType.System_Object)
                                : classBinder.ResolveType(decl.TypeAnnotation.Type);
                            _ = new SourceFieldSymbol(
                                decl.Identifier.Text,
                                fieldType,
                                isStatic: false,
                                isLiteral: false,
                                constantValue: null,
                                classSymbol,
                                classSymbol,
                                parentNamespace.AsSourceNamespace(),
                                [decl.GetLocation()],
                                [decl.GetReference()]
                            );
                        }
                        break;
                    }
                case MethodDeclarationSyntax methodDecl:
                    {
                        var returnType = methodDecl.ReturnType is null
                            ? Compilation.GetSpecialType(SpecialType.System_Void)
                            : classBinder.ResolveType(methodDecl.ReturnType.Type);
                        var methodSymbol = new SourceMethodSymbol(
                            methodDecl.Identifier.Text,
                            returnType,
                            ImmutableArray<SourceParameterSymbol>.Empty,
                            classSymbol,
                            classSymbol,
                            parentNamespace.AsSourceNamespace(),
                            [methodDecl.GetLocation()],
                            [methodDecl.GetReference()],
                            isStatic: false);

                        var parameters = new List<SourceParameterSymbol>();
                        foreach (var p in methodDecl.ParameterList.Parameters)
                        {
                            var pType = classBinder.ResolveType(p.TypeAnnotation!.Type);
                            var pSymbol = new SourceParameterSymbol(
                                p.Identifier.Text,
                                pType,
                                methodSymbol,
                                classSymbol,
                                parentNamespace.AsSourceNamespace(),
                                [p.GetLocation()],
                                [p.GetReference()]
                            );
                            parameters.Add(pSymbol);
                        }

                        methodSymbol.SetParameters(parameters);
                        break;
                    }
                case ConstructorDeclarationSyntax ctorDecl:
                    {
                        var idToken = ctorDecl.Identifier;
                        var isPrimary = !idToken.HasValue;
                        var name = isPrimary ? ".ctor" : idToken.Value.Text;
                        var returnType = isPrimary ? Compilation.GetSpecialType(SpecialType.System_Void) : classSymbol;
                        var methodKind = isPrimary ? MethodKind.Constructor : MethodKind.Ordinary;

                        var ctorSymbol = new SourceMethodSymbol(
                            name,
                            returnType,
                            ImmutableArray<SourceParameterSymbol>.Empty,
                            classSymbol,
                            classSymbol,
                            parentNamespace.AsSourceNamespace(),
                            [ctorDecl.GetLocation()],
                            [ctorDecl.GetReference()],
                            isStatic: false,
                            methodKind: methodKind);

                        var parameters = new List<SourceParameterSymbol>();
                        foreach (var p in ctorDecl.ParameterList.Parameters)
                        {
                            var pType = classBinder.ResolveType(p.TypeAnnotation!.Type);
                            var pSymbol = new SourceParameterSymbol(
                                p.Identifier.Text,
                                pType,
                                ctorSymbol,
                                classSymbol,
                                parentNamespace.AsSourceNamespace(),
                                [p.GetLocation()],
                                [p.GetReference()]
                            );
                            parameters.Add(pSymbol);
                        }

                        ctorSymbol.SetParameters(parameters);
                        break;
                    }
            }
        }
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