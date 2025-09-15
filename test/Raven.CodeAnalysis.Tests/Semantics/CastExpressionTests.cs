using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class CastExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void ExplicitCast_Numeric_NoDiagnostic()
    {
        string code = """
        let x = (double)1
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_Invalid_ProducesDiagnostic()
    {
        string code = """
        let s = (string)1
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1503").WithAnySpan().WithArguments("int", "string")
        ]);
        verifier.Verify();
    }
}
