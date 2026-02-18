using Raven.CodeAnalysis.Diagnostics;

namespace Raven.CodeAnalysis.Testing;

public abstract class AnalyzerTestBase
{
    protected AnalyzerVerifier<TAnalyzer> CreateAnalyzerVerifier<TAnalyzer>(string testCode, IEnumerable<DiagnosticResult>? expectedDiagnostics = null, IEnumerable<string>? disabledDiagnostics = null, bool enableSuggestions = false)
        where TAnalyzer : DiagnosticAnalyzer, new()
    {
        return new AnalyzerVerifier<TAnalyzer>
        {
            Test = new Test
            {
                TestCode = testCode,
                ExpectedDiagnostics = expectedDiagnostics?.ToList() ?? [],
                DisabledDiagnostics = disabledDiagnostics?.ToList() ?? [],
                State = new TestState
                {
                    EnableSuggestions = enableSuggestions
                }
            }
        };
    }
}
