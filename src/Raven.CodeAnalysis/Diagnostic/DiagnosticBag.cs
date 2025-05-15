using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class DiagnosticBag
{
    private readonly List<Diagnostic> _diagnostics;

    public bool IsEmpty => _diagnostics.Count == 0;

    public DiagnosticBag()
    {
        _diagnostics = new();
    }

    internal DiagnosticBag(IEnumerable<Diagnostic> enumerable)
    {
        _diagnostics = enumerable.ToList();
    }

    public void Report(Diagnostic diagnostic)
    {
        _diagnostics.Add(diagnostic);
    }

    public void AddRange(IEnumerable<Diagnostic> diagnostics)
    {
        _diagnostics.AddRange(diagnostics);
    }

    public ImmutableArray<Diagnostic> ToImmutableArray()
    {
        return [.. _diagnostics];
    }

    internal void ReportInvalidMemberAccess(string memberName, Location location)
    {
        throw new NotImplementedException();
    }
}