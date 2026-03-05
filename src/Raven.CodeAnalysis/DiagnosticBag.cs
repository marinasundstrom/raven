using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

namespace Raven.CodeAnalysis;

public class DiagnosticBag
{
    private readonly ConcurrentDictionary<Diagnostic, byte> _diagnostics;
    private int _reportingDisabledCount;

    public bool IsEmpty => _diagnostics.IsEmpty;

    public DiagnosticBag()
    {
        _diagnostics = new();
    }

    internal DiagnosticBag(IEnumerable<Diagnostic> enumerable)
    {
        _diagnostics = new ConcurrentDictionary<Diagnostic, byte>(
            enumerable.Select(static diagnostic => new KeyValuePair<Diagnostic, byte>(diagnostic, 0)));
    }

    public void Report(Diagnostic diagnostic)
    {
        if (Volatile.Read(ref _reportingDisabledCount) > 0)
            return;

        _diagnostics.TryAdd(diagnostic, 0);
    }

    public IEnumerable<Diagnostic> AsEnumerable()
    {
        return _diagnostics.Keys;
    }

    public void ClearDiagnostics(TextSpan textSpan)
    {
        foreach (var diagnostic in _diagnostics.Keys)
        {
            if (diagnostic.Location != null && diagnostic.Location.SourceSpan.IntersectsWith(textSpan))
                _diagnostics.TryRemove(diagnostic, out _);
        }
    }

    public IDisposable CreateNonReportingScope()
    {
        Interlocked.Increment(ref _reportingDisabledCount);
        return new NonReportingScopeDisposable(this);
    }

    public void ResumeReporting()
    {
        Interlocked.Decrement(ref _reportingDisabledCount);
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
