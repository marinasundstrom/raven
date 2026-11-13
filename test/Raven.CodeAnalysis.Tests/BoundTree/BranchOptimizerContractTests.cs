using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BranchOptimizerContractTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void AdjacentGotoRemovalPreservesLabelAndBlockMetadata(bool scope)
    {
        var target = Label("target");
        var label = new BoundLabeledStatement(target, Statement());
        var local = Make<LocalSymbol>(SymbolKind.Local, "resource");
        var block = new BoundBlockStatement([new BoundGotoStatement(target), label], [local], scope);
        var result = Rewrite(block);
        Assert.Same(label, Assert.Single(result.Statements));
        Assert.Same(local, Assert.Single(result.LocalsToDispose));
        Assert.Equal(scope, result.IntroduceILScope);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void InversionPreservesConditionIdentityAndRedirectsTarget(bool jumpIfTrue)
    {
        var fallthrough = Label("fallthrough");
        var target = Label("target");
        var condition = Value();
        var label = new BoundLabeledStatement(fallthrough, Statement());
        var result = Rewrite(new BoundBlockStatement([
            new BoundConditionalGotoStatement(fallthrough, condition, jumpIfTrue),
            new BoundGotoStatement(target), label]));
        var statements = result.Statements.ToArray();
        Assert.Equal(2, statements.Length);
        var branch = Assert.IsType<BoundConditionalGotoStatement>(statements[0]);
        Assert.Same(condition, branch.Condition);
        Assert.Same(target, branch.Target);
        Assert.Equal(!jumpIfTrue, branch.JumpIfTrue);
        Assert.Same(label, statements[1]);
    }

    [Fact]
    public void BackwardRemovalHandlesConsecutiveRedundantGotos()
    {
        var target = Label("target");
        var label = new BoundLabeledStatement(target, Statement());
        var result = Rewrite(new BoundBlockStatement([
            new BoundGotoStatement(target), new BoundGotoStatement(target), label]));
        Assert.Same(label, Assert.Single(result.Statements));
    }

    [Fact]
    public void SameNamedDistinctLabelsDoNotMatch()
    {
        var first = Label("same");
        var second = Label("same");
        var label = new BoundLabeledStatement(second, Statement());
        var jump = new BoundGotoStatement(first);
        var direct = Rewrite(new BoundBlockStatement([jump, label]));
        Assert.Equal(new BoundStatement[] { jump, label }, direct.Statements);
        var conditional = new BoundConditionalGotoStatement(first, Value(), true);
        var inverted = Rewrite(new BoundBlockStatement([conditional, jump, label]));
        Assert.Equal(new BoundStatement[] { conditional, jump, label }, inverted.Statements);
    }

    [Fact]
    public void InterveningStatementsPreventGotoRemoval()
    {
        var target = Label("target");
        var jump = new BoundGotoStatement(target);
        var intervening = Statement();
        var label = new BoundLabeledStatement(target, Statement());
        var result = Rewrite(new BoundBlockStatement([jump, intervening, label]));
        Assert.Equal(new BoundStatement[] { jump, intervening, label }, result.Statements);
    }

    [Fact]
    public void NestedBlocksAreRewrittenBeforeTheirParent()
    {
        var target = Label("target");
        var label = new BoundLabeledStatement(target, Statement());
        var child = new BoundBlockStatement([new BoundGotoStatement(target), label], introduceILScope: false);
        var result = Rewrite(new BoundBlockStatement([child]));
        var rewrittenChild = Assert.IsType<BoundBlockStatement>(Assert.Single(result.Statements));
        Assert.False(rewrittenChild.IntroduceILScope);
        Assert.Same(label, Assert.Single(rewrittenChild.Statements));
    }

    [Fact]
    public void EmptySingleAndNonBranchStatementsRemainIntact()
    {
        foreach (var statements in new BoundStatement[][] { [], [Statement()], [Statement(), Statement()] })
            Assert.Equal(statements, Rewrite(new BoundBlockStatement(statements)).Statements);
    }

    private static LabelSymbol Label(string name) => Make<LabelSymbol>(SymbolKind.Label, name);

    private static BoundExpression Value() => new BoundDefaultValueExpression(Named("Runtime", "Example.Value"));

    private static BoundStatement Statement() => new BoundExpressionStatement(Value());

    private static BoundBlockStatement Rewrite(BoundBlockStatement block)
        => Assert.IsType<BoundBlockStatement>(BranchOptimizer.Rewrite(block));
}
