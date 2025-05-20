using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private readonly DiagnosticBag _diagnostics;
    private readonly Dictionary<SyntaxNode, SymbolInfo> _bindings = new();

    public SemanticModel(Compilation compilation, SyntaxTree syntaxTree)
    {
        _diagnostics = new DiagnosticBag();
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

        foreach (var globalStmt in root.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            var binder = Compilation.BinderFactory.GetBinder(globalStmt);
            binder.BindStatement(globalStmt.Statement);
        }
    }

    public SymbolInfo GetSymbolInfo(SyntaxNode node, CancellationToken cancellationToken = default)
    {
        // TODO: Remove caching
        if (_bindings.TryGetValue(node, out var symbolInfo))
            return symbolInfo;

        // NEW: Ask binder to bind the node
        var binder = Compilation.BinderFactory.GetBinder(node);
        var info = binder.BindSymbol(node);
        _bindings[node] = info;
        return info;
    }

    public ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        var binder = Compilation.BinderFactory.GetBinder(node);
        var info = binder.BindSymbol(node);
        return info.Symbol;
    }

    public ImmutableArray<ISymbol> LookupSymbols(int position,
        INamespaceOrTypeSymbol container, string name, bool includeReducedExtensionMethods)
    {
        throw new NotImplementedException();
    }
}
