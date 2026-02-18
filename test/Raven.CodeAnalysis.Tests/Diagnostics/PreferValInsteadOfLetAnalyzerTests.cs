using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferValInsteadOfLetAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void LetBinding_ReportsDiagnostic()
    {
        const string code = "let x = 1";

        var verifier = CreateAnalyzerVerifier<PreferValInsteadOfLetAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferValInsteadOfLetAnalyzer.PreferValInsteadOfLetDiagnosticId)
                    .WithLocation(1, 1)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
