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
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void AutoPropertyAccessors_SameLineSemicolons_DoNotReportDiagnostic()
    {
        const string code = "class C { public required Name: string { get; init; } }";

        var verifier = CreateAnalyzerVerifier<PreferNewLineBetweenDeclarationsAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }
}
