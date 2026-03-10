using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ConversionDiagnosticsTests : DiagnosticTestBase
{
    [Fact]
    public void ImplicitConversionFailure_WithExplicitConversionAvailable_ReportsHintDiagnostic()
    {
        const string code = """
        class Flag {
            static func explicit(value: Flag) -> bool { return true }
        }

        func Test(flag: Flag) -> () {
            if flag {
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFromTypeToType.Id).WithAnySpan().WithArguments("Flag", "bool"),
            new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id).WithAnySpan().WithArguments("Flag", "bool"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_WithImplicitConversion_ReportsRedundantCastDiagnostic()
    {
        const string code = """
        val x = (double)1
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.RedundantExplicitCast.Id).WithAnySpan().WithArguments("int", "double"),
        ]);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitCast_Required_DoesNotReportRedundantCastDiagnostic()
    {
        const string code = """
        val x = (int)1.5
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
