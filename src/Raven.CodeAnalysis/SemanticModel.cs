using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly BinderFactory _binderFactory;
    private readonly DiagnosticBag _diagnostics;
    private readonly List<ISymbol> _symbols = new List<ISymbol>();
    private readonly List<ISymbol> _localSymbols = new List<ISymbol>();
    private readonly Dictionary<SyntaxNode, SymbolInfo> _bindings = new();
    private readonly Dictionary<TypeSyntax, INamespaceSymbol> _imports = new();
    private readonly Dictionary<string, ITypeSymbol> _keywordTypeSymbols = new();

    public SemanticModel(Compilation compilation, List<ISymbol> symbols, SyntaxTree syntaxTree)
    {
        _binderFactory = new BinderFactory(compilation);

        _symbols = symbols;
        _diagnostics = new DiagnosticBag();
        Compilation = compilation;
        SyntaxTree = syntaxTree;

        CreateModel();
    }

    private void CreateModel()
    {
        // Optional: preload binder for compilation unit to cache imports
        var root = SyntaxTree.GetRoot();
        _ = _binderFactory.GetBinder(root);
    }

    public Compilation Compilation { get; }

    public SyntaxTree SyntaxTree { get; }

    private DiagnosticBag Diagnostics => _diagnostics;

    public IImmutableList<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        EnsureDiagnosticsCollected();

        return _binderFactory
             .GetAllBinders()
             .SelectMany(b => b.Diagnostics.ToImmutableArray())
             //.DistinctBy(d => (d.Descriptor.Id, d.Location, d.GetMessage()))
             .ToImmutableArray();
    }

    private void EnsureDiagnosticsCollected()
    {
        var root = SyntaxTree.GetRoot();

        foreach (var globalStmt in root.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            var binder = _binderFactory.GetBinder(globalStmt);
            binder.BindStatement(globalStmt.Statement);
        }
    }

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        if (_bindings.TryGetValue(node, out var symbolInfo))
            return symbolInfo;

        // NEW: Ask binder to bind the node
        var binder = _binderFactory.GetBinder(node);
        var info = binder.BindSymbol(node);
        _bindings[node] = info;
        return info;
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        var binder = _binderFactory.GetBinder(node);
        var info = binder.BindSymbol(node);
        return info.Symbol;
    }

    public ImmutableArray<ISymbol> LookupSymbols(int position,
        INamespaceOrTypeSymbol container, string name, bool includeReducedExtensionMethods)
    {
        throw new NotImplementedException();
    }

    private void Bind(SyntaxNode node, ISymbol symbol)
    {
        _bindings[node] = new SymbolInfo(symbol);
    }

    private void Bind(SyntaxNode node, MapToCandidateReason reason, params IEnumerable<ISymbol> symbols)
    {
        _bindings[node] = new SymbolInfo(reason, symbols.ToImmutableArray());
    }

    private ITypeSymbol? InferLiteralType(LiteralExpressionSyntax literal)
    {
        // Example inference logic for literals
        return literal.Token.Value switch
        {
            int => GetTypeSymbol("System.Int32"),
            string => GetTypeSymbol("System.String"),
            double => GetTypeSymbol("System.Double"),
            bool => GetTypeSymbol("System.Boolean"),
            _ => null
        };
    }

    private ITypeSymbol? GetTypeSymbol(string typeName)
    {
        return _symbols
            .OfType<ITypeSymbol>()
            .FirstOrDefault(x => x.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) == typeName);
    }
}
