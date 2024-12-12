using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class DiagnosticBag
{
    private readonly List<Diagnostic> _diagnostics = new();

    public void Add(Diagnostic diagnostic)
    {
        _diagnostics.Add(diagnostic);
    }

    public void AddRange(IEnumerable<Diagnostic> diagnostics)
    {
        _diagnostics.AddRange(diagnostics);
    }

    public ImmutableArray<Diagnostic> ToImmutableArray()
    {
        return [.._diagnostics];
    }
}