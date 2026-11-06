using System;
using System.Collections.Generic;

namespace Raven.CodeAnalysis;

internal static class NumericPatternIntervals
{
    public static IReadOnlyList<NumericInterval> Range(NumericInterval domain, Int128 low, Int128 high)
    {
        var clipped = new NumericInterval(Int128.Max(low, domain.Low), Int128.Min(high, domain.High));
        return clipped.IsEmpty ? [] : [clipped];
    }

    public static bool TryCompare(NumericInterval domain, BoundComparisonPatternOperator op, Int128 value,
        out IReadOnlyList<NumericInterval> intervals)
    {
        intervals = [];
        switch (op)
        {
            case BoundComparisonPatternOperator.Equals:
                intervals = Range(domain, value, value);
                break;
            case BoundComparisonPatternOperator.NotEquals:
                intervals = domain.Subtract(new(value, value));
                break;
            case BoundComparisonPatternOperator.GreaterThan:
                if (value < domain.High)
                    intervals = Range(domain, value + 1, domain.High);
                break;
            case BoundComparisonPatternOperator.GreaterThanOrEqual:
                intervals = Range(domain, value, domain.High);
                break;
            case BoundComparisonPatternOperator.LessThan:
                if (value > domain.Low)
                    intervals = Range(domain, domain.Low, value - 1);
                break;
            case BoundComparisonPatternOperator.LessThanOrEqual:
                intervals = Range(domain, domain.Low, value);
                break;
            default:
                return false;
        }
        return true;
    }

    public static IReadOnlyList<NumericInterval> Intersect(IReadOnlyList<NumericInterval> left, IReadOnlyList<NumericInterval> right)
    {
        var result = new List<NumericInterval>();
        foreach (var first in left)
        {
            foreach (var second in right)
            {
                var overlap = new NumericInterval(Int128.Max(first.Low, second.Low), Int128.Min(first.High, second.High));
                if (!overlap.IsEmpty)
                    result.Add(overlap);
            }
        }
        return result;
    }
}
