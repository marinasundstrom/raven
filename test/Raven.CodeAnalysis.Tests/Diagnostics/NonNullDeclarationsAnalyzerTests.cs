using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class NonNullDeclarationsAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void NullableLocalType_ReportsDiagnosticWithReplacementSuggestion()
    {
        const string code = """
func Test() {
    var value: int? = null
}
""";

        var verifier = CreateAnalyzerVerifier<NonNullDeclarationsAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(NonNullDeclarationsAnalyzer.DiagnosticId)
                    .WithSpan(2, 16, 2, 20)
                    .WithArguments("Option<int>", "int?")
            ],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }

    [Fact]
    public void NullableUnionDeclarationType_ReportsDiagnosticWithReplacementSuggestion()
    {
        const string code = """
func Test() {
    var value: int | null = null
}
""";

        var verifier = CreateAnalyzerVerifier<NonNullDeclarationsAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(NonNullDeclarationsAnalyzer.DiagnosticId)
                    .WithSpan(2, 16, 2, 26)
                    .WithArguments("Option<int>", "int?")
            ],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }
}
