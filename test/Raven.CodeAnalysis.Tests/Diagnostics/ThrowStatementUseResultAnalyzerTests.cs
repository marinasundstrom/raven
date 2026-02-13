using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class ThrowStatementUseResultAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ThrowStatement_ReportsDiagnosticOnEntireStatement()
    {
        const string code = """
func Test() {
    throw Exception("boom")
}
""";

        var verifier = CreateAnalyzerVerifier<ThrowStatementUseResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(ThrowStatementUseResultAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 28)
            ],
            disabledDiagnostics: ["RAV1014", "RAV0103"]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowExpression_ReportsDiagnosticOnEntireExpression()
    {
        const string code = """
func Test(name: string?) -> string {
    return name ?? throw Exception("missing")
}
""";

        var verifier = CreateAnalyzerVerifier<ThrowStatementUseResultAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(ThrowStatementUseResultAnalyzer.DiagnosticId)
                    .WithSpan(2, 20, 2, 46)
            ],
            disabledDiagnostics: ["RAV1014", "RAV0103"]);

        verifier.Verify();
    }

    [Fact]
    public void NoThrowStatement_NoDiagnostic()
    {
        const string code = """
func Test() -> int {
    return 42
}
""";

        var verifier = CreateAnalyzerVerifier<ThrowStatementUseResultAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: ["RAV1014"]);

        verifier.Verify();
    }
}
