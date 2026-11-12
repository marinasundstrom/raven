using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundFunctionStatementContractTests
{
    [Fact]
    public void MethodAndUpdatePreserveReferenceIdentity()
    {
        var method = Make<MethodSymbol>(SymbolKind.Method, "First");
        var replacement = Make<MethodSymbol>(SymbolKind.Method, "Second");
        var node = new BoundFunctionStatement(method);
        Assert.Same(method, node.Method);
        Assert.Null(node.Symbol);
        Assert.Same(node, node.Update(method));
        var changed = node.Update(replacement);
        Assert.NotSame(node, changed);
        Assert.Same(replacement, changed.Method);
        Assert.Same(method, node.Method);
    }

    [Fact]
    public void VisitorDispatchesToFunctionStatement()
    {
        var node = new BoundFunctionStatement(Make<MethodSymbol>(SymbolKind.Method, "Run"));
        var visitor = new RecordingVisitor();
        visitor.Visit(node);
        Assert.Same(node, visitor.Seen);
        Assert.Equal(42, new ResultVisitor().Visit(node));
    }

    [Fact]
    public void DefaultWalkerTreatsFunctionDeclarationAsLeaf()
    {
        var node = new BoundFunctionStatement(Make<MethodSymbol>(SymbolKind.Method, "Run"));
        var walker = new RecordingWalker();
        walker.Visit(node);
        Assert.Same(node, Assert.Single(walker.Nodes));
    }

    private sealed class RecordingVisitor : BoundTreeVisitor
    {
        public BoundFunctionStatement? Seen { get; private set; }
        public override void VisitFunctionStatement(BoundFunctionStatement node) => Seen = node;
    }

    private sealed class ResultVisitor : BoundTreeVisitor<int>
    {
        public override int VisitFunctionStatement(BoundFunctionStatement node) => 42;
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        public List<BoundNode> Nodes { get; } = [];
        public override void DefaultVisit(BoundNode node) => Nodes.Add(node);
    }
}
