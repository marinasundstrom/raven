using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public class DiagnosticBag
{
    private readonly HashSet<Diagnostic> _diagnostics;
    private bool _dontReport;

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
        if (_dontReport)
            return;

        _diagnostics.Add(diagnostic);
    }

    public IEnumerable<Diagnostic> AsEnumerable()
    {
        return _diagnostics;
    }

    public void ClearDiagnostics(TextSpan textSpan)
    {
        _diagnostics.RemoveWhere(x => x.Location != null && x.Location.SourceSpan.IntersectsWith(textSpan));
    }

    public IDisposable CreateNonReportingScope()
    {
        _dontReport = true;
        return new NonReportingScopeDisposable(this);
    }

    public void ResumeReporting()
    {
        _dontReport = false;
    }
}

internal class NonReportingScopeDisposable : IDisposable
{
    private DiagnosticBag _diagnosticBag;

    public NonReportingScopeDisposable(DiagnosticBag diagnosticBag)
    {
        _diagnosticBag = diagnosticBag;
    }

    public void Dispose()
    {
        _diagnosticBag.ResumeReporting();
    }
}
