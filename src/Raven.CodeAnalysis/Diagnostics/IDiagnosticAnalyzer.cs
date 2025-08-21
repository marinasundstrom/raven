namespace Raven.CodeAnalysis.Diagnostics;

/// <summary>Defines an analyzer that can produce diagnostics for a compilation.</summary>
public interface IDiagnosticAnalyzer
{
    IEnumerable<Diagnostic> Analyze(Compilation compilation, CancellationToken cancellationToken = default);
}
