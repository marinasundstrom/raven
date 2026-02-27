using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferNewLineBetweenDeclarationsAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void TypeDeclarations_OnSameLineWithSemicolon_ReportsDiagnostic()
    {
        const string code = "class A {}; trait T for A {}";

        var verifier = CreateAnalyzerVerifier<PreferNewLineBetweenDeclarationsAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(PreferNewLineBetweenDeclarationsAnalyzer.DiagnosticId)
                    .WithLocation(1, 11)
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

}
