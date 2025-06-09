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

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (_symbolMappings.TryGetValue(node, out var symbolInfo))
            return symbolInfo;

        var binder = GetBinder(node);
        var info = binder.BindReferencedSymbol(node);
        _symbolMappings[node] = info;
        return info;
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        var binder = GetBinder(node);
        return binder.BindDeclaredSymbol(node);
    }

    public ImmutableArray<ISymbol> LookupSymbols(int position,
        INamespaceOrTypeSymbol container, string name, bool includeReducedExtensionMethods)
    {
        throw new NotImplementedException();
    }

    public TypeInfo GetTypeInfo(ExpressionSyntax expr)
    {
        var binder = GetBinder(expr);

        var boundExpr = binder.BindExpression(expr);

        if (boundExpr is null || boundExpr is BoundErrorExpression)
            return new TypeInfo(null, null);

        return new TypeInfo(boundExpr.Type, boundExpr.GetConvertedType());
    }

    internal BoundNode GetBoundNode(SyntaxNode node)
    {
        var binder = GetBinder(node);
        return binder.GetOrBind(node);
    }

    internal BoundExpression GetBoundNode(ExpressionSyntax node)
    {
        return (BoundExpression)GetBoundNode((SyntaxNode)node);
    }

    internal T? GetBoundNode<T>(SyntaxNode node)
        where T : BoundNode
    {
        return GetBoundNode(node) as T;
    }

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
        // Determine if there's a file-scoped namespace
        var declaredNamespace = cu.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();

        INamespaceSymbol targetNamespace;

        if (declaredNamespace is not null)
        {
            targetNamespace = Compilation.GetNamespaceSymbol(declaredNamespace.Name.ToString())
                              ?? throw new Exception("Namespace not found");

            parentBinder = new NamespaceBinder(parentBinder, targetNamespace, Compilation);
        }
        else
        {
            targetNamespace = Compilation.GlobalNamespace;
            parentBinder = new NamespaceBinder(parentBinder, targetNamespace, Compilation);
        }

        // Process import/using directives
        var imports = cu.Imports
            .Select(i => Compilation.GetNamespaceSymbol(i.NamespaceOrType.ToString()))
            .OfType<INamespaceSymbol>()
            .ToList();

        var importBinder = new ImportBinder(parentBinder, imports);

        var programClassSymbol = new SynthesizedProgramClassSymbol(Compilation, targetNamespace.AsSourceNamespace(), [cu.GetLocation()], [cu.GetReference()]);

        var mainMethodSymbol = new SynthesizedMainMethodSymbol(programClassSymbol, [cu.GetLocation()], [cu.GetReference()]);

        var semanticModel = this;

        var topLevelBinder = new TopLevelBinder(importBinder, semanticModel, mainMethodSymbol);

        foreach (var member in cu.Members.OfType<BaseTypeDeclarationSyntax>())
        {
            if (member is EnumDeclarationSyntax enumDeclaration)
            {
                var enumType = new SourceNamedTypeSymbol(enumDeclaration.Identifier.Text, Compilation.GetTypeByMetadataName("System.Enum"), TypeKind.Enum, targetNamespace.AsSourceNamespace(), null, targetNamespace.AsSourceNamespace(),
                    [enumDeclaration.GetLocation()], [enumDeclaration.GetReference()]);

                int value = 0;
                foreach (var enumMember in enumDeclaration.Members)
                {
                    new SourceFieldSymbol(enumMember.Identifier.Text, enumType, true, true, value, enumType, enumType, targetNamespace.AsSourceNamespace(),
                         [enumMember.GetLocation()], [enumMember.GetReference()]);
                    value++;
                }
            }
        }

        // 🟢 Step 1: Predeclare all local functions
        foreach (var stmt in cu.Members.OfType<GlobalStatementSyntax>())
        {
            if (stmt.Statement is LocalFunctionStatementSyntax localFunc)
            {
                var binder = GetBinder(localFunc, topLevelBinder);
                if (binder is LocalFunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    topLevelBinder.DeclareLocalFunction(symbol);
                }
            }
        }

        // 🟢 Step 2: Bind all statements
        foreach (var stmt in cu.Members.OfType<GlobalStatementSyntax>())
        {
            topLevelBinder.BindGlobalStatement(stmt);
        }

        return topLevelBinder;
    }

    internal BoundNode? TryGetCachedBoundNode(SyntaxNode node)
    => _boundNodeCache.TryGetValue(node, out var bound) ? bound : null;

    internal void CacheBoundNode(SyntaxNode node, BoundNode bound)
        => _boundNodeCache[node] = bound;
}
