using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundTreeRewriterContractTests
{
    [Fact]
    public void NullAndUnchangedLeafRetainIdentity()
    {
        var rewriter = new Identity();
        Assert.Null(rewriter.Visit(null));
        Assert.Null(rewriter.VisitExpression(null));
        Assert.Null(rewriter.VisitSymbol(null));
        var leaf = new BoundDefaultValueExpression(Named("A", "Example.Value"));
        Assert.Same(leaf, rewriter.Visit(leaf));
        var exit = new BoundBreakStatement();
        Assert.Same(exit, rewriter.Visit(exit));
    }

    [Fact]
    public void GeneratedRewriteUpdatesParentAndPreservesOriginal()
    {
        var type = Named("A", "Example.Value");
        var original = new BoundDefaultValueExpression(type);
        var replacement = new BoundDefaultValueExpression(type);
        var statement = new BoundReturnStatement(original);
        var result = Assert.IsType<BoundReturnStatement>(new Replace(original, replacement).Visit(statement));
        Assert.NotSame(statement, result);
        Assert.Same(original, statement.Expression);
        Assert.Same(replacement, result.Expression);
    }

    [Fact]
    public void GeneratedBlockRewriteTraversesListsAndPreservesDisposalSymbols()
    {
        var type = Named("A", "Example.Value");
        var value = new BoundDefaultValueExpression(type);
        var replacement = new BoundDefaultValueExpression(type);
        var local = Make<LocalSymbol>(SymbolKind.Local, "resource");
        var block = new BoundBlockStatement([new BoundReturnStatement(value)], [local], false);
        var rewritten = Assert.IsType<BoundBlockStatement>(new Replace(value, replacement).Visit(block));
        Assert.Same(replacement, Assert.IsType<BoundReturnStatement>(Assert.Single(rewritten.Statements)).Expression);
        Assert.Same(local, Assert.Single(rewritten.LocalsToDispose));
        Assert.False(rewritten.IntroduceILScope);
        Assert.Same(value, Assert.IsType<BoundReturnStatement>(Assert.Single(block.Statements)).Expression);
    }

    [Fact]
    public void NodeListsRemainLazyAndPreserveOrder()
    {
        var type = Named("A", "Example.Value");
        var first = new BoundDefaultValueExpression(type);
        var second = new BoundDefaultValueExpression(type);
        var replacement = new BoundDefaultValueExpression(type);
        var visits = new Replace(first, replacement);
        var result = visits.VisitList<BoundExpression>([first, second]);
        Assert.Equal(0, visits.Visits);
        Assert.Equal<BoundExpression>([replacement, second], result);
        Assert.Equal(2, visits.Visits);
    }

    [Fact]
    public void CollectionComprehensionRetainsIdentityOrRebuildsOnlyWhenChanged()
    {
        var type = Named("A", "Example.Value");
        var source = new BoundDefaultValueExpression(type);
        var selector = new BoundDefaultValueExpression(type);
        var local = Make<LocalSymbol>(SymbolKind.Local, "item");
        var node = new BoundCollectionComprehensionExpression(type, source, local, null, selector, type);
        Assert.Same(node, new Identity().Visit(node));
        var replacement = new BoundDefaultValueExpression(type);
        var changed = Assert.IsType<BoundCollectionComprehensionExpression>(new Replace(selector, replacement).Visit(node));
        Assert.Same(source, changed.Source);
        Assert.Same(replacement, changed.Selector);
        Assert.Same(local, changed.IterationLocal);
        Assert.Null(changed.Condition);
        Assert.Same(selector, node.Selector);
    }

    [Fact]
    public void PatternCategoryPreservesOriginalWhenVisitorReturnsNull()
    {
        var node = new BoundDiscardPattern(Named("A", "Example.Value"));
        Assert.Same(node, new RemoveDiscard().Visit(node));
        Assert.Same(node, new RemoveDiscard().VisitExpression(node));
    }

    private sealed class Identity : BoundTreeRewriter { }

    private sealed class Replace(BoundExpression original, BoundExpression replacement) : BoundTreeRewriter
    {
        public int Visits { get; private set; }

        public override BoundNode? VisitDefaultValueExpression(BoundDefaultValueExpression node)
        {
            Visits++;
            return ReferenceEquals(node, original) ? replacement : node;
        }
    }

    private sealed class RemoveDiscard : BoundTreeRewriter
    {
        public override BoundNode? VisitDiscardPattern(BoundDiscardPattern node) => null;
    }
}
