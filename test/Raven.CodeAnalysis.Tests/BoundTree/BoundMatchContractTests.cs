using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundMatchContractTests
{
    [Theory]
    [InlineData(0, false)]
    [InlineData(0, true)]
    [InlineData(1, false)]
    [InlineData(1, true)]
    [InlineData(2, false)]
    [InlineData(2, true)]
    public void MatchRootsVisitInputPatternGuardAndResultInOrder(int rootKind, bool guarded)
    {
        var type = Named("A", "Example.Value");
        var local = Make<LocalSymbol>(SymbolKind.Local, "input");
        Set(local, "get_Type", type);
        var input = new BoundLocalAccess(local);
        var guard = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, type);
        var result = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 2, type);
        var pattern = new BoundDiscardPattern(type);
        var arm = new BoundMatchArm(pattern, guarded ? guard : null, result);
        var arms = ImmutableArray.Create(arm);
        var statement = new BoundMatchStatement(input, arms);
        var expression = new BoundMatchExpression(input, arms, type);
        Assert.Same(local, statement.Symbol);
        Assert.Same(statement, statement.Update(input, arms));
        Assert.Same(expression, expression.Update(input, arms, type));
        var walker = new RecordingWalker();
        walker.Visit(rootKind switch { 0 => expression, 1 => statement, _ => arm });
        string[] expected = guarded ? ["pattern", "1", "2"] : ["pattern", "2"];
        if (rootKind != 2)
            expected = ["input", .. expected];
        Assert.Equal(expected, walker.Trace);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void UnionCaseKeepsCarrierTypeAndOptionalConstructor(bool hasConstructor)
    {
        var union = Named("A", "Example.Result");
        var caseType = Named("A", "Example.Success");
        var constructor = hasConstructor ? Make<MethodSymbol>(SymbolKind.Method, ".ctor") : null;
        var argument = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 7, union);
        var arguments = ImmutableArray.Create<BoundExpression>(argument);
        var expression = new BoundUnionCaseExpression(union, caseType, constructor, arguments);
        Assert.Same(union, expression.Type);
        Assert.Same(union, expression.UnionType);
        Assert.Same(caseType, expression.CaseType);
        Assert.Same(constructor, expression.Symbol);
        Assert.Same(constructor, expression.CaseConstructor);
        Assert.Same(expression, expression.Update(union, caseType, constructor, arguments));
        Assert.EndsWith(" as Result", expression.ToString());
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Equal(new[] { "7" }, walker.Trace);
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<string> Trace { get; } = [];

        public override void VisitLocalAccess(BoundLocalAccess node) => Trace.Add("input");
        public override void VisitDiscardPattern(BoundDiscardPattern node) => Trace.Add("pattern");
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Trace.Add(node.Value!.ToString()!);
    }
}
