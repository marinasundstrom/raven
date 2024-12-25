namespace Raven.CodeAnalysis.Tests.Framework;

public abstract class DiagnosticTestBase
{
    protected DiagnosticVerifier CreateVerifier(string testCode, IEnumerable<DiagnosticResult>? expectedDiagnostics = null, IEnumerable<string>? disabledDiagnostics = null)
    {
        return new DiagnosticVerifier
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