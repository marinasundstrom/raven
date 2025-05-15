using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class DiagnosticBag
{
    private readonly HashSet<Diagnostic> _diagnostics;

    public bool IsEmpty => _diagnostics.Count == 0;

    public DiagnosticBag()
    {
        _diagnostics = new();
    }

    internal DiagnosticBag(IEnumerable<Diagnostic> enumerable)
    {
        _diagnostics = [.. enumerable];
    }

    public void Report(Diagnostic diagnostic)
    {
        _diagnostics.Add(diagnostic);
    }

    public ImmutableArray<Diagnostic> ToImmutableArray()
    {
        return [.. _diagnostics];
    }
}