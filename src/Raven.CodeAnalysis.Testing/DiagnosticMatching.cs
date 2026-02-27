using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Testing;

internal static class DiagnosticMatching
{
    private static readonly ImmutableHashSet<string> DefaultDisabledDiagnostics = ImmutableHashSet.Create(
        "RAV0908");

    public static HashSet<string> BuildDisabledDiagnostics(IEnumerable<string> additionalDisabledDiagnostics)
    {
        var disabled = DefaultDisabledDiagnostics.ToHashSet();
        foreach (var diagnosticId in additionalDisabledDiagnostics)
            disabled.Add(diagnosticId);
        return disabled;
    }

    public static bool MessageArgumentsMatch(IReadOnlyList<object> expectedArguments, IReadOnlyList<object?> actualArguments)
    {
        if (expectedArguments.Count != actualArguments.Count)
            return false;

        for (var i = 0; i < expectedArguments.Count; i++)
        {
            var expected = NormalizeMessageArgument(expectedArguments[i]);
            var actual = NormalizeMessageArgument(actualArguments[i]);
            if (!string.Equals(expected, actual, StringComparison.Ordinal))
                return false;
        }

        return true;
    }

    private static string NormalizeMessageArgument(object? value)
    {
        return (value?.ToString() ?? string.Empty)
            .Replace("→", "->", StringComparison.Ordinal);
    }
}
