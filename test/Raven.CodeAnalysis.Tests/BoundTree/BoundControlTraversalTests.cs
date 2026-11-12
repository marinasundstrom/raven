using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundControlTraversalTests
{
    [Theory]
    [InlineData("initializer")]
    [InlineData("assignment")]
    [InlineData("content")]
    public void GenericVisitDispatchesInitializerRoots(string kind)
    {
        var type = Named("A", "Example.Value");
        var value = Literal(1, type);
        var member = Make<FieldSymbol>(SymbolKind.Field, "Value");
        BoundNode root = kind switch
        {
            "assignment" => new BoundObjectInitializerAssignmentEntry(member, value),
            "content" => new BoundObjectInitializerExpressionEntry(value),
            _ => new BoundObjectInitializer([new BoundObjectInitializerExpressionEntry(value)])
        };
        var walker = new RecordingWalker();
        walker.Visit(root);
        Assert.Same(value, Assert.Single(walker.Literals));
    }

    [Fact]
    public void LockVisitsMonitorThenBody()
    {
        var type = Named("A", "Example.Value");
        var monitor = Literal(1, type);
        var body = Literal(2, type);
        var walker = new RecordingWalker();
        var statement = new BoundLockStatement(monitor, new BoundExpressionStatement(body));
        Assert.Same(statement, statement.Update(monitor, statement.Body));
        walker.Visit(statement);
        Assert.Equal<BoundLiteralExpression>([monitor, body], walker.Literals);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void TryVisitsCatchPatternGuardAndFinally(bool filtered)
    {
        var type = Named("A", "Example.Value");
        var values = Enumerable.Range(0, 4).Select(value => Literal(value, type)).ToArray();
        var pattern = new BoundDiscardPattern(type);
        var clause = new BoundCatchClause(type, null, filtered ? pattern : null, filtered ? values[1] : null,
            new BoundBlockStatement([new BoundExpressionStatement(values[2])]));
        var statement = new BoundTryStatement(new BoundBlockStatement([new BoundExpressionStatement(values[0])]),
            ImmutableArray.Create(clause), new BoundBlockStatement([new BoundExpressionStatement(values[3])]));
        var walker = new RecordingWalker();
        walker.Visit(statement);
        Assert.Equal<BoundLiteralExpression>(filtered ? values : values.Where((_, index) => index != 1), walker.Literals);
        Assert.Equal(filtered ? 1 : 0, walker.Patterns.Count);
        if (filtered)
            Assert.Same(pattern, Assert.Single(walker.Patterns));
    }

    private static BoundLiteralExpression Literal(int value, TypeSymbol type)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, type);

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Literals { get; } = [];
        internal List<BoundDiscardPattern> Patterns { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Literals.Add(node);
        public override void VisitDiscardPattern(BoundDiscardPattern node) => Patterns.Add(node);
    }
}
