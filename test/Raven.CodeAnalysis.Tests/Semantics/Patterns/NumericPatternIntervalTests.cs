namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "Shared")]
public sealed class NumericPatternIntervalTests
{
    [Fact]
    public void ComparisonsAgreeWithIntegralOperatorsInsideAndOutsideTheDomain()
    {
        var domain = new NumericInterval(-2, 2);
        foreach (var op in Enum.GetValues<BoundComparisonPatternOperator>())
        {
            foreach (Int128 operand in new Int128[] { Int128.MinValue, -3, -2, 0, 2, 3, Int128.MaxValue })
            {
                Assert.True(NumericPatternIntervals.TryCompare(domain, op, operand, out var intervals));
                Assert.All(intervals, interval => Assert.True(!interval.IsEmpty && interval.Low >= domain.Low && interval.High <= domain.High));
                for (var value = -2; value <= 2; value++)
                {
                    var expected = op switch
                    {
                        BoundComparisonPatternOperator.Equals => value == operand,
                        BoundComparisonPatternOperator.NotEquals => value != operand,
                        BoundComparisonPatternOperator.LessThan => value < operand,
                        BoundComparisonPatternOperator.LessThanOrEqual => value <= operand,
                        BoundComparisonPatternOperator.GreaterThan => value > operand,
                        BoundComparisonPatternOperator.GreaterThanOrEqual => value >= operand,
                        _ => throw new ArgumentOutOfRangeException()
                    };
                    Assert.Equal(expected, intervals.Any(interval => value >= interval.Low && value <= interval.High));
                }
            }
        }
        Assert.False(NumericPatternIntervals.TryCompare(domain, (BoundComparisonPatternOperator)100, 0, out var unknown));
        Assert.Empty(unknown);
    }

    [Fact]
    public void ComplementOfIntersectionPreservesBothOuterSegmentsAndInnerGap()
    {
        var domain = new NumericInterval(0, 10);
        var overlaps = NumericPatternIntervals.Intersect([new(-2, 2), new(5, 9)], [new(1, 6)]);
        Assert.Equal<NumericInterval>([new(1, 2), new(5, 6)], overlaps);
        Assert.Equal<NumericInterval>([new(0, 0), new(3, 4), new(7, 10)], NumericMatchCoverage.Complement(domain, overlaps));
        Assert.Empty(NumericPatternIntervals.Range(domain, 8, 4));
    }
}
