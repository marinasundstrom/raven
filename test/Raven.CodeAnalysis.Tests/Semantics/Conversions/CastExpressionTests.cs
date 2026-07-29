using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class CastExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void ExplicitCast_Numeric_ReportsRedundantCastDiagnostic()
    {
        string code = """
        let x = (double)1
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RedundantExplicitCast.Id).WithAnySpan().WithArguments("int", "double")
        ]);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_Invalid_ProducesDiagnostic()
    {
        string code = """
        let s = (string)1
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id).WithAnySpan().WithArguments("int", "string")
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
    public void ExplicitCast_UnionToMemberType_NoDiagnostic()
    {
        const string code = """
        let value: Either<int, string> = 42
        let left = (int)value

        union Either<T1, T2>(T1 | T2)
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_WithAdditionalParentheses_ReportsRedundantCastDiagnostic()
    {
        string code = """
        let value = ((double)1)
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RedundantExplicitCast.Id).WithAnySpan().WithArguments("int", "double")
        ]);
        verifier.Verify();
    }
}
