using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ThrowStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void ThrowExpressionMustDeriveFromException()
    {
        var code = """
func main() {
    throw 42;
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1020").WithSpan(2, 11, 2, 13)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowStatementInExpressionContext_ReportsDiagnostic()
    {
        var code = """
func main() {
    let value = {
        throw System.InvalidOperationException("fail")
        ()
    };
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1907").WithSpan(4, 9, 4, 14)
            ]);

        verifier.Verify();
    }
}
