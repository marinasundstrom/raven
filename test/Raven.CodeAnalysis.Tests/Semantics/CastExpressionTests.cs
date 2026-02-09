using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class CastExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void ExplicitCast_Numeric_NoDiagnostic()
    {
        string code = """
        val x = (double)1
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_Invalid_ProducesDiagnostic()
    {
        string code = """
        val s = (string)1
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

        val type = typeof(System.String)
        val members = type.GetMembers()
        val first = members[0]
        val method = (MethodInfo)first
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_DowncastReferenceType_FullyQualified_NoDiagnostic()
    {
        string code = """
        val type = typeof(System.String)
        val members = type.GetMembers()
        val first = members[0]
        val method = (System.Reflection.MethodInfo)first
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_WithAdditionalParentheses_NoDiagnostic()
    {
        string code = """
        val value = ((double)1)
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
