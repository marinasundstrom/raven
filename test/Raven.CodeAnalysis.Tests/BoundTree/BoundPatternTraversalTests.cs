using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundPatternTraversalTests
{
    [Fact]
    public void ConjunctionExposesBothBindingsAndUpdatesWithoutMutation()
    {
        var type = Named("A", "Example.Item");
        BoundDeclarationPattern Declare(string name)
        {
            var local = Make<LocalSymbol>(SymbolKind.Local, name);
            Set(local, "get_Type", type);
            return new BoundDeclarationPattern(type, new BoundSingleVariableDesignator(local));
        }

        var left = Declare("first");
        var right = Declare("second");
        var conjunction = new BoundAndPattern(left, right);
        Assert.Equal<BoundDesignator>([left.Designator, right.Designator], conjunction.GetDesignators());
        Assert.Same(conjunction, conjunction.Update(left, right));
        var discard = new BoundDeclarationPattern(type, new BoundDiscardDesignator(type));
        var updated = conjunction.Update(discard, right);
        Assert.Same(left, conjunction.Left);
        Assert.Same(discard, updated.Left);
        Assert.Same(right.Designator, Assert.Single(updated.GetDesignators()));
        var guard = new BoundGuardedPattern(conjunction, null, null);
        Assert.Equal<BoundDesignator>([left.Designator, right.Designator], guard.GetDesignators());
    }

    [Fact]
    public void WalkerVisitsWrappedPatternAndGuardOperandsInOrder()
    {
        var type = Named("A", "Example.Number");
        BoundExpression Literal(int value) => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type);
        var range = new BoundRangePattern(type, Literal(1), Literal(2), isUpperExclusive: false);
        var comparison = new BoundComparisonPattern(type, BoundComparisonPatternOperator.GreaterThan, Literal(3));
        var method = Make<MethodSymbol>(SymbolKind.Method, "TryGet");
        var inner = new BoundUnionMemberPattern(type, type, method, new BoundNotPattern(range));
        var or = new BoundOrPattern(inner, new BoundRangePattern(type, null, null));
        var guarded = new BoundGuardedPattern(or, comparison, Literal(4));
        var walker = new RecordingWalker();
        walker.Visit(new BoundExpressionStatement(guarded.RequireValue()));
        Assert.Equal([1, 2, 3, 4], walker.Values.ToArray());

        var visitor = new ReturningVisitor();
        Assert.Equal("guard", visitor.Visit(guarded));
        Assert.Equal("range", visitor.Visit(range));
        Assert.Equal("comparison", visitor.Visit(comparison));
        Assert.Equal("member", visitor.Visit(inner));
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<int> Values { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add((int)node.Value);
    }

    private sealed class ReturningVisitor : BoundTreeVisitor<string>
    {
        public override string VisitGuardedPattern(BoundGuardedPattern node) => "guard";
        public override string VisitRangePattern(BoundRangePattern node) => "range";
        public override string VisitComparisonPattern(BoundComparisonPattern node) => "comparison";
        public override string VisitUnionMemberPattern(BoundUnionMemberPattern node) => "member";
    }
}
