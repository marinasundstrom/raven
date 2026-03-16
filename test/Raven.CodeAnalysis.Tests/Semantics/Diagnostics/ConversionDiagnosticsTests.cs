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

    [Fact]
    public void OpenArray_ToFixedSizeArray_ReportsSpecificDiagnostic()
    {
        const string code = """
        val values: int[] = [1, 2, 3]
        val fixed: int[3] = values
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertOpenArrayToFixedSizeArray.Id)
                .WithAnySpan()
                .WithArguments("int[]", "int[3]")
        ]);

        verifier.Verify();
    }

    [Fact]
    public void FixedSizeArray_ToDifferentFixedSizeArray_ReportsSpecificDiagnostic()
    {
        const string code = """
        val values: int[2] = [1, 2]
        val fixed: int[3] = values
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.CannotConvertFixedSizeArrayToDifferentSize.Id)
                .WithAnySpan()
                .WithArguments("int[2]", "int[3]", 2, 3)
        ]);

        verifier.Verify();
    }
}
