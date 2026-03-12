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

    [Fact]
    public void AssignmentConversionFailure_WithExplicitConversionAvailable_ReportsHintDiagnostic()
    {
        const string code = """
        class Foo {
            static func explicit(value: Foo) -> string { return "" }
        }

        func Main() -> () {
            val foo = Foo()
            val text: string = foo
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotAssignFromTypeToType.Id).WithAnySpan().WithArguments("Foo", "string"),
            new DiagnosticResult(CompilerDiagnostics.ExplicitConversionExists.Id).WithAnySpan().WithArguments("Foo", "string"),
        ]);

        verifier.Verify();
    }
}
