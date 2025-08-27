using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis.Testing;

public abstract class AnalyzerTestBase
{
    protected AnalyzerVerifier<TAnalyzer> CreateAnalyzerVerifier<TAnalyzer>(string testCode, IEnumerable<DiagnosticResult>? expectedDiagnostics = null, IEnumerable<string>? disabledDiagnostics = null)
        where TAnalyzer : DiagnosticAnalyzer, new()
    {
        return new AnalyzerVerifier<TAnalyzer>
        {
            Test = new Test
            {
                TestCode = testCode,
                ExpectedDiagnostics = expectedDiagnostics?.ToList() ?? [],
                DisabledDiagnostics = disabledDiagnostics?.ToList() ?? []
            }
        };
    }
}
