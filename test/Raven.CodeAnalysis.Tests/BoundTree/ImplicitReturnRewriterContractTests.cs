using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class ImplicitReturnRewriterContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TrailingExpressionPreservesBlockScopeAndDisposalMetadata(bool introduceScope)
    {
        var type = Named("A", "Example.Value");
        var unit = Named("A", "Example.Unit");
        var value = new BoundDefaultValueExpression(type);
        var local = Make<LocalSymbol>(SymbolKind.Local, "resource");
        var statement = new BoundExpressionStatement(value);
        var body = new BoundBlockStatement([statement], [local], introduceScope);
        Assert.True(ImplicitReturnRewriter.IsImplicitReturnCandidate(type, unit, body, out var candidate));
        Assert.Same(statement, candidate);
        var rewritten = ImplicitReturnRewriter.RewriteIfNeeded(type, unit, body);
        Assert.Same(value, Assert.IsType<BoundReturnStatement>(Assert.Single(rewritten.Statements)).Expression);
        Assert.Equal(introduceScope, rewritten.IntroduceILScope);
        Assert.Same(local, Assert.Single(rewritten.LocalsToDispose));
        Assert.Same(statement, Assert.Single(body.Statements));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConditionalBranchesPreserveTheirBlockScope(bool introduceScope)
    {
        var type = Named("A", "Example.Value");
        var unit = Named("A", "Example.Unit");
        var value = new BoundDefaultValueExpression(type);
        var thenBranch = new BoundBlockStatement([new BoundExpressionStatement(value)], introduceILScope: introduceScope);
        var conditional = new BoundIfStatement(value, thenBranch, new BoundExpressionStatement(value));
        var body = new BoundBlockStatement([conditional], introduceILScope: introduceScope);
        var rewritten = ImplicitReturnRewriter.RewriteIfNeeded(type, unit, body);
        Assert.Equal(introduceScope, rewritten.IntroduceILScope);
        var result = Assert.IsType<BoundIfStatement>(Assert.Single(rewritten.Statements));
        var resultThen = Assert.IsType<BoundBlockStatement>(result.ThenNode);
        Assert.Equal(introduceScope, resultThen.IntroduceILScope);
        Assert.IsType<BoundReturnStatement>(Assert.Single(resultThen.Statements));
        Assert.IsType<BoundReturnStatement>(result.ElseNode);
        Assert.Same(value, result.Condition);
    }

    [Theory]
    [InlineData("void")]
    [InlineData("unit-return")]
    [InlineData("unit-expression")]
    [InlineData("error-expression")]
    [InlineData("untyped-expression")]
    public void IneligibleExpressionPreservesBodyAndClearsCandidate(string scenario)
    {
        var type = Named("A", "Example.Value");
        var unit = Named("A", "Example.Unit");
        var returnType = scenario == "unit-return" ? unit : type;
        if (scenario == "void")
            Set(type, "get_SpecialType", SpecialType.System_Void);
        var expressionType = scenario == "unit-expression" ? unit : Named("A", "Example.Expression");
        if (scenario == "error-expression")
            Set(expressionType, "get_TypeKind", TypeKind.Error);
        BoundExpression expression = scenario == "untyped-expression"
            ? new BoundNamespaceExpression(Make<NamespaceSymbol>(SymbolKind.Namespace, "Example"))
            : new BoundDefaultValueExpression(expressionType);
        var body = new BoundBlockStatement([new BoundExpressionStatement(expression)]);
        Assert.False(ImplicitReturnRewriter.IsImplicitReturnCandidate(returnType, unit, body, out var candidate));
        Assert.Null(candidate);
        Assert.Same(body, ImplicitReturnRewriter.RewriteIfNeeded(returnType, unit, body));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MissingOrIneligibleElseKeepsTheWholeBodyUnchanged(bool hasElse)
    {
        var type = Named("A", "Example.Value");
        var unit = Named("A", "Example.Unit");
        var value = new BoundDefaultValueExpression(type);
        var thenBranch = new BoundExpressionStatement(value);
        var conditional = new BoundIfStatement(value, thenBranch,
            hasElse ? new BoundExpressionStatement(new BoundDefaultValueExpression(unit)) : null);
        var body = new BoundBlockStatement([conditional]);
        Assert.Same(body, ImplicitReturnRewriter.RewriteIfNeeded(type, unit, body));
        Assert.Same(thenBranch, conditional.ThenNode);
    }

    [Fact]
    public void NestedBareConditionalBranchesAreRewrittenRecursively()
    {
        var type = Named("A", "Example.Value");
        var unit = Named("A", "Example.Unit");
        var value = new BoundDefaultValueExpression(type);
        var expression = new BoundExpressionStatement(value);
        var nested = new BoundIfStatement(value, expression, expression);
        var body = new BoundBlockStatement([new BoundIfStatement(value, nested, expression)]);
        var rewritten = ImplicitReturnRewriter.RewriteIfNeeded(type, unit, body);
        var outer = Assert.IsType<BoundIfStatement>(Assert.Single(rewritten.Statements));
        var inner = Assert.IsType<BoundIfStatement>(outer.ThenNode);
        Assert.IsType<BoundReturnStatement>(inner.ThenNode);
        Assert.IsType<BoundReturnStatement>(inner.ElseNode);
        Assert.IsType<BoundReturnStatement>(outer.ElseNode);
    }

    [Fact]
    public void EmptyBodyAndExplicitReturnRetainIdentity()
    {
        var type = Named("A", "Example.Value");
        var unit = Named("A", "Example.Unit");
        foreach (var body in new[] { new BoundBlockStatement([]), new BoundBlockStatement([new BoundReturnStatement(null)]) })
        {
            Assert.False(ImplicitReturnRewriter.IsImplicitReturnCandidate(type, unit, body, out var candidate));
            Assert.Null(candidate);
            Assert.Same(body, ImplicitReturnRewriter.RewriteIfNeeded(type, unit, body));
        }
    }
}
