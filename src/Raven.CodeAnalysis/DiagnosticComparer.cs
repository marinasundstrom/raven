using System;
using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed class DiagnosticComparer : IComparer<Diagnostic>
{
    public static DiagnosticComparer Instance { get; } = new();

    private DiagnosticComparer()
    {
    }

    public int Compare(Diagnostic? x, Diagnostic? y)
    {
        if (ReferenceEquals(x, y))
            return 0;
        if (x is null)
            return -1;
        if (y is null)
            return 1;

        var locationComparison = CompareLocations(x.Location, y.Location);
        if (locationComparison != 0)
            return locationComparison;

        var idComparison = string.Compare(x.Id, y.Id, StringComparison.Ordinal);
        if (idComparison != 0)
            return idComparison;

        var severityComparison = x.Severity.CompareTo(y.Severity);
        if (severityComparison != 0)
            return severityComparison;

        var messageComparison = string.Compare(x.GetMessage(), y.GetMessage(), StringComparison.Ordinal);
        if (messageComparison != 0)
            return messageComparison;

        var suppressionComparison = x.IsSuppressed.CompareTo(y.IsSuppressed);
        if (suppressionComparison != 0)
            return suppressionComparison;

        return CompareProperties(x, y);
    }

    private static int CompareLocations(Location? x, Location? y)
    {
        if (ReferenceEquals(x, y))
            return 0;
        if (x is null)
            return -1;
        if (y is null)
            return 1;

        var lineSpanX = x.GetLineSpan();
        var lineSpanY = y.GetLineSpan();

        var pathComparison = string.Compare(lineSpanX.Path ?? string.Empty, lineSpanY.Path ?? string.Empty, StringComparison.OrdinalIgnoreCase);
        if (pathComparison != 0)
            return pathComparison;

        var startLineComparison = lineSpanX.StartLinePosition.Line.CompareTo(lineSpanY.StartLinePosition.Line);
        if (startLineComparison != 0)
            return startLineComparison;

        var startCharacterComparison = lineSpanX.StartLinePosition.Character.CompareTo(lineSpanY.StartLinePosition.Character);
        if (startCharacterComparison != 0)
            return startCharacterComparison;

        var endLineComparison = lineSpanX.EndLinePosition.Line.CompareTo(lineSpanY.EndLinePosition.Line);
        if (endLineComparison != 0)
            return endLineComparison;

        var endCharacterComparison = lineSpanX.EndLinePosition.Character.CompareTo(lineSpanY.EndLinePosition.Character);
        if (endCharacterComparison != 0)
            return endCharacterComparison;

        var sourceSpanStartComparison = x.SourceSpan.Start.CompareTo(y.SourceSpan.Start);
        if (sourceSpanStartComparison != 0)
            return sourceSpanStartComparison;

        var sourceSpanLengthComparison = x.SourceSpan.Length.CompareTo(y.SourceSpan.Length);
        if (sourceSpanLengthComparison != 0)
            return sourceSpanLengthComparison;

        return x.Kind.CompareTo(y.Kind);
    }

    private static int CompareProperties(Diagnostic x, Diagnostic y)
    {
        var countComparison = x.Properties.Count.CompareTo(y.Properties.Count);
        if (countComparison != 0)
            return countComparison;

        using var left = x.Properties.OrderBy(static property => property.Key, StringComparer.Ordinal).GetEnumerator();
        using var right = y.Properties.OrderBy(static property => property.Key, StringComparer.Ordinal).GetEnumerator();

        while (left.MoveNext() && right.MoveNext())
        {
            var keyComparison = string.Compare(left.Current.Key, right.Current.Key, StringComparison.Ordinal);
            if (keyComparison != 0)
                return keyComparison;

            var valueComparison = string.Compare(left.Current.Value, right.Current.Value, StringComparison.Ordinal);
            if (valueComparison != 0)
                return valueComparison;
        }

        return 0;
    }
}
