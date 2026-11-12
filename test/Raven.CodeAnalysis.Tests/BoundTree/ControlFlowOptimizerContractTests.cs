using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class ControlFlowOptimizerContractTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void LiteralConditionsSelectExpressionAndStatementBranches(bool condition)
    {
        var type = Named("A", "Example.Value");
        var first = new BoundDefaultValueExpression(type);
        var second = new BoundDefaultValueExpression(type);
        var literal = Literal(condition);
        Assert.Same(condition ? first : second,
            ControlFlowOptimizer.Rewrite(new BoundIfExpression(literal, first, second)));
        var thenStatement = new BoundExpressionStatement(first);
        var elseStatement = new BoundExpressionStatement(second);
        Assert.Same(condition ? thenStatement : elseStatement,
            ControlFlowOptimizer.Rewrite(new BoundIfStatement(literal, thenStatement, elseStatement)));
    }

    [Theory]
    [InlineData(true, true)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(false, false)]
    public void ConditionalGotoPreservesTargetOrBecomesScopelessEmptyBlock(bool condition, bool jumpIfTrue)
    {
        var target = Make<LabelSymbol>(SymbolKind.Label, "target");
        var result = ControlFlowOptimizer.Rewrite(new BoundConditionalGotoStatement(target, Literal(condition), jumpIfTrue));
        if (condition == jumpIfTrue)
            Assert.Same(target, Assert.IsType<BoundGotoStatement>(result).Target);
        else
            AssertEmpty(result);
    }

    [Fact]
    public void FalseConditionWithoutElseRetainsExpressionButEliminatesStatement()
    {
        var value = Literal(true);
        var expression = new BoundIfExpression(Literal(false), value);
        Assert.Same(expression, ControlFlowOptimizer.Rewrite(expression));
        AssertEmpty(ControlFlowOptimizer.Rewrite(new BoundIfStatement(Literal(false), new BoundExpressionStatement(value))));
    }

    [Theory]
    [InlineData("unknown")]
    [InlineData("conversion")]
    [InlineData("as")]
    public void NonconstantConditionsRemainUnchanged(string wrapper)
    {
        var literal = Literal(true);
        BoundExpression condition = wrapper switch
        {
            "conversion" => new BoundConversionExpression(literal, literal.Type!, new Conversion(isImplicit: true)),
            "as" => new BoundAsExpression(literal, literal.Type!, new Conversion(isImplicit: true)),
            _ => new BoundDefaultValueExpression(literal.Type!),
        };
        var value = new BoundExpressionStatement(literal);
        var statement = new BoundIfStatement(condition, value);
        var expression = new BoundIfExpression(condition, literal, literal);
        var jump = new BoundConditionalGotoStatement(Make<LabelSymbol>(SymbolKind.Label, "target"), condition, true);
        Assert.Same(statement, ControlFlowOptimizer.Rewrite(statement));
        Assert.Same(expression, ControlFlowOptimizer.Rewrite(expression));
        Assert.Same(jump, ControlFlowOptimizer.Rewrite(jump));
    }

    [Fact]
    public void IdentityWrappersAndNestedBranchesAreRewrittenRecursively()
    {
        var literal = Literal(true);
        var identity = new Conversion(isImplicit: true, isIdentity: true);
        var wrapped = new BoundParenthesizedExpression(new BoundAsExpression(
            new BoundConversionExpression(literal, literal.Type!, identity), literal.Type!, identity));
        var expected = new BoundExpressionStatement(literal);
        var nested = new BoundIfStatement(Literal(false), new BoundBlockStatement([]), expected);
        Assert.Same(expected, ControlFlowOptimizer.Rewrite(new BoundIfStatement(wrapped, nested)));
    }

    private static BoundLiteralExpression Literal(bool value)
        => new(value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            value, Named("A", "Example.Boolean"));

    private static void AssertEmpty(BoundNode node)
    {
        var block = Assert.IsType<BoundBlockStatement>(node);
        Assert.Empty(block.Statements);
        Assert.Empty(block.LocalsToDispose);
        Assert.False(block.IntroduceILScope);
    }
}
