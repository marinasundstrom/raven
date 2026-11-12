using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundLeafContractTests
{
    [Fact]
    public void ErrorExpressionNormalizesCandidatesAndPreservesDiagnosticContext()
    {
        var type = Named("A", "Example.Value");
        var candidate = Make<MethodSymbol>(SymbolKind.Method, "Read");
        var empty = new BoundErrorExpression(type);
        Assert.False(empty.Candidates.IsDefault);
        Assert.Empty(empty.Candidates);
        var candidates = ImmutableArray.Create<Symbol>(candidate);
        var expression = new BoundErrorExpression(type, candidate, BoundExpressionReason.OtherError, candidates);
        Assert.Same(type, expression.Type);
        Assert.Same(candidate, expression.Symbol);
        Assert.Equal(BoundExpressionReason.OtherError, expression.Reason);
        Assert.Same(candidate, Assert.Single(expression.Candidates));
        Assert.Same(expression, expression.Update(type, candidate, expression.Reason, candidates));
        var changed = expression.Update(type, null, expression.Reason, candidates);
        Assert.NotSame(expression, changed);
        Assert.Null(changed.Symbol);
        Assert.Same(candidate, expression.Symbol);
    }

    [Fact]
    public void VariableAndUnitPreserveTheirTypeContracts()
    {
        var type = Named("A", "Example.Value");
        var local = Make<LocalSymbol>(SymbolKind.Local, "value");
        Set(local, "get_Type", type);
        var expression = new BoundVariableExpression(local);
        Assert.Same(local, expression.Variable);
        Assert.Same(local, expression.Symbol);
        Assert.Same(type, expression.Type);
        Assert.Same(expression, expression.Update(local));
        var unit = new BoundUnitExpression(type, BoundExpressionReason.OtherError);
        Assert.Same(type, unit.UnitType);
        Assert.Equal(BoundExpressionReason.OtherError, unit.Reason);
        Assert.Same(unit, unit.Update(type, unit.Reason));
    }

    [Fact]
    public void StackAllocationVisitsTheCountExpression()
    {
        var elementType = Named("A", "Example.Element");
        var allocationType = Named("A", "Example.Allocation");
        var count = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 4, elementType);
        var allocation = new BoundStackAllocExpression(elementType, count, allocationType);
        Assert.Same(elementType, allocation.ElementType);
        Assert.Same(allocationType, allocation.Type);
        Assert.Same(allocationType, allocation.AllocationType);
        Assert.Same(allocation, allocation.Update(elementType, count, allocationType));
        var walker = new RecordingWalker();
        walker.Visit(allocation);
        Assert.Same(count, Assert.Single(walker.Values));
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Values { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add(node);
    }
}
