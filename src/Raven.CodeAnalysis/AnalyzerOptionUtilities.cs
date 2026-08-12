using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

internal static class AnalyzerOptionUtilities
{
    private static readonly char[] Separators = [';', ',', ' '];

    public static ImmutableHashSet<string> ParseAnalyzerNameSet(string? value)
    {
        if (string.IsNullOrWhiteSpace(value))
            return EmptyAnalyzerNameSet();

        return ParseAnalyzerNameSet(value.Split(Separators, StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries));
    }

    public static ImmutableHashSet<string> ParseAnalyzerNameSet(IEnumerable<string>? values)
    {
        if (values is null)
            return EmptyAnalyzerNameSet();

        return values
            .Where(static value => !string.IsNullOrWhiteSpace(value))
            .Select(static value => value.Trim())
            .ToImmutableHashSet(StringComparer.OrdinalIgnoreCase);
    }

    public static string FormatAnalyzerNameSet(IEnumerable<string> values)
        => string.Join(";", ParseAnalyzerNameSet(values).OrderBy(static value => value, StringComparer.OrdinalIgnoreCase));

    public static bool IsAnalyzerDisabled(Type analyzerType, ImmutableHashSet<string> disabledAnalyzers)
    {
        if (disabledAnalyzers.IsEmpty)
            return false;

        if (disabledAnalyzers.Contains(analyzerType.Name) ||
            disabledAnalyzers.Contains(analyzerType.FullName ?? string.Empty))
        {
            return true;
        }

        return analyzerType.Name is nameof(Diagnostics.UnusedLocalAnalyzer) or nameof(Diagnostics.UnusedParameterAnalyzer) &&
               disabledAnalyzers.Contains(nameof(Diagnostics.UnusedVariableAnalyzer));
    }

    public static bool IsAnalyzerEnabled(
        Type analyzerType,
        ImmutableHashSet<string> enabledAnalyzers,
        string? category = null)
    {
        if (enabledAnalyzers.IsEmpty)
            return false;

        if (enabledAnalyzers.Contains("*") ||
            enabledAnalyzers.Contains("all") ||
            enabledAnalyzers.Contains(analyzerType.Name) ||
            enabledAnalyzers.Contains(analyzerType.FullName ?? string.Empty) ||
            (category is not null && enabledAnalyzers.Contains($"category:{category}")))
        {
            return true;
        }

        return analyzerType.Name is nameof(Diagnostics.UnusedLocalAnalyzer) or nameof(Diagnostics.UnusedParameterAnalyzer) &&
               enabledAnalyzers.Contains(nameof(Diagnostics.UnusedVariableAnalyzer));
    }

    private static ImmutableHashSet<string> EmptyAnalyzerNameSet()
        => ImmutableHashSet<string>.Empty.WithComparer(StringComparer.OrdinalIgnoreCase);
}
