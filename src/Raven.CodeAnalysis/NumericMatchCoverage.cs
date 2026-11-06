using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;

namespace Raven.CodeAnalysis;

internal sealed class NumericMatchCoverage(NumericInterval domain)
{
    private List<NumericInterval> _remaining = [domain];

    public bool IsComplete => _remaining.Count == 0;

    public static IReadOnlyList<NumericInterval> Complement(NumericInterval domain, IReadOnlyList<NumericInterval> intervals)
    {
        var coverage = new NumericMatchCoverage(domain);
        foreach (var interval in intervals)
            coverage.Cover(interval);
        return coverage._remaining;
    }

    public void Cover(NumericInterval interval)
    {
        var next = new List<NumericInterval>();
        foreach (var remaining in _remaining)
            next.AddRange(remaining.Subtract(interval));
        _remaining = next;
    }

    public ImmutableArray<string> GetMissingCases(bool isChar)
    {
        var builder = ImmutableArray.CreateBuilder<string>(_remaining.Count);
        foreach (var interval in _remaining)
        {
            if (interval.Low == interval.High)
                builder.Add(FormatValue(interval.Low, isChar));
            else if (interval.Low == domain.Low)
                builder.Add("..=" + FormatValue(interval.High, isChar));
            else if (interval.High == domain.High)
                builder.Add(FormatValue(interval.Low, isChar) + "..");
            else
                builder.Add(FormatValue(interval.Low, isChar) + "..=" + FormatValue(interval.High, isChar));
        }
        return builder.MoveToImmutable();
    }

    private static string FormatValue(Int128 value, bool isChar)
    {
        if (!isChar)
            return value.ToString(CultureInfo.InvariantCulture);

        var ch = (char)value;
        if (ch is '\'' or '\\' || char.IsControl(ch) || char.IsSurrogate(ch) || ch is '\u2028' or '\u2029')
            return "'\\u" + value.ToString("X4", CultureInfo.InvariantCulture) + "'";
        return $"'{ch}'";
    }
}
