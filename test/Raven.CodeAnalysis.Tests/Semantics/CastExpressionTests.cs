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

    [Fact]
    public void ExplicitCast_DowncastReferenceType_NoDiagnostic()
    {
        string code = """
        import System.Reflection.*

        let type = typeof(System.String)
        let members = type.GetMembers()
        let first = members[0]
        let method = (MethodInfo)first
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_DowncastReferenceType_FullyQualified_NoDiagnostic()
    {
        string code = """
        let type = typeof(System.String)
        let members = type.GetMembers()
        let first = members[0]
        let method = (System.Reflection.MethodInfo)first
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_WithAdditionalParentheses_NoDiagnostic()
    {
        string code = """
        let value = ((double)1)
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
