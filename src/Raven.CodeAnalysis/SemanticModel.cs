using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly Dictionary<SyntaxNode, SymbolInfo> _bindings = new();

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        Compilation = compilation;
        SyntaxTree = syntaxTree;

        CreateModel();
    }

    private void CreateModel()
    {
        // Optional: preload binder for compilation unit to cache imports
        var root = SyntaxTree.GetRoot();
        _ = Compilation.BinderFactory.GetBinder(root);
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    public IImmutableList<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        EnsureDiagnosticsCollected();

        return Compilation.BinderFactory
             .GetAllBinders(SyntaxTree)
             .SelectMany(b => b.Diagnostics.AsEnumerable())
             .ToImmutableArray();
    }

    private void EnsureDiagnosticsCollected()
    {
        var root = SyntaxTree.GetRoot();

        var topLevelBinder = Compilation.BinderFactory.GetBinder(root) as TopLevelBinder;

        foreach (var globalStmt in root.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            topLevelBinder.BindGlobalStatement(globalStmt);
        }
    }

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (_bindings.TryGetValue(node, out var symbolInfo))
            return symbolInfo;

        var binder = Compilation.BinderFactory.GetBinder(node);
        var info = binder.BindReferencedSymbol(node);
        _bindings[node] = info;
        return info;
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        var binder = Compilation.BinderFactory.GetBinder(node);
        return binder.BindDeclaredSymbol(node);
    }

    public ImmutableArray<ISymbol> LookupSymbols(int position,
        INamespaceOrTypeSymbol container, string name, bool includeReducedExtensionMethods)
    {
        throw new NotImplementedException();
    }

    public TypeInfo GetTypeInfo(ExpressionSyntax expr)
    {
        var binder = Compilation.BinderFactory.GetBinder(expr);

        var boundExpr = binder.BindExpression(expr);

        if (boundExpr is null || boundExpr is BoundErrorExpression)
            return new TypeInfo(null, null);

        return new TypeInfo(boundExpr.Type, boundExpr.GetConvertedType());
    }
}
