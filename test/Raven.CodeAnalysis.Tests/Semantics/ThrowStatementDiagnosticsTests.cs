using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ThrowStatementDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void ThrowExpressionOperandMustDeriveFromException()
    {
        var code = """
func Main() {
    throw 42;
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1020").WithSpan(2, 11, 2, 13).WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowExpressionMustDeriveFromException()
    {
        var code = """
func Main() {
    val value = "name" ?? throw 42;
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1020").WithSpan(2, 33, 2, 35).WithArguments("int")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ThrowStatementInBlockExpressionInitializer_IsAllowed()
    {
        var code = """
func Main() {
    val value = {
        throw System.InvalidOperationException("fail")
        ()
    };
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV0162").WithSpan(4, 9, 4, 11)
            ]);

        verifier.Verify();
    }
}
