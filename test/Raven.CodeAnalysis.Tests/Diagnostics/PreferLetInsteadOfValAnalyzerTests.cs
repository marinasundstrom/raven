using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferLetInsteadOfValAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ValBinding_ReportsDiagnostic()
    {
        const string code = "val x = 1";

        var verifier = CreateAnalyzerVerifier<PreferLetInsteadOfValAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferLetInsteadOfValAnalyzer.PreferLetInsteadOfValDiagnosticId)
                    .WithLocation(1, 1)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void LetBinding_DoesNotReportDiagnostic()
    {
        const string code = "let x = 1";

        var verifier = CreateAnalyzerVerifier<PreferLetInsteadOfValAnalyzer>(
            code,
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }
}
