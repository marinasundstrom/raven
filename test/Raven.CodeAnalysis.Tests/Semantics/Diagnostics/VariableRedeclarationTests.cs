using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class VariableRedeclarationTests : DiagnosticTestBase
{
    [Fact]
    public void DuplicateGlobalVariable_ReportsWarning()
    {
        var code = """
val x: int = 1
val x: int = 2
""";
        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0168").WithSpan(2, 5, 2, 6).WithArguments("x").WithSeverity(DiagnosticSeverity.Warning)
            ]);
        verifier.Verify();
    }

    [Fact]
    public void DuplicateLocalVariable_ReportsWarning()
    {
        var code = """
func Main() {
    val x: int = 1
    val x: int = 2
}
""";
        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0168").WithSpan(3, 9, 3, 10).WithArguments("x").WithSeverity(DiagnosticSeverity.Warning)
            ]);
        verifier.Verify();
    }

    [Fact]
    public void ShadowingInInnerBlock_Warns()
    {
        var code = """
val x: int = 1
if true {
    val x: int = 2
}
""";
        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0168").WithSpan(3, 9, 3, 10).WithArguments("x").WithSeverity(DiagnosticSeverity.Warning)
            ]);
        verifier.Verify();
    }

    [Fact]
    public void LocalShadowingParameter_Warns()
    {
        var code = """
func demo(x: int) {
    val x = x + 1
}
""";
        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV0168").WithSpan(2, 9, 2, 10).WithArguments("x").WithSeverity(DiagnosticSeverity.Warning)
            ]);
        verifier.Verify();
    }

    [Fact]
    public void VariableUseBeforeDeclaration_ReportsError()
    {
        var code = """
func Main() {
    x
    val x = 1
}
""";
        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.VariableUsedBeforeDeclaration.Id).WithSpan(2, 5, 2, 6).WithArguments("x")
            ]);
        verifier.Verify();
    }

    [Fact]
    public void VariableUseBeforeDeclarationShadowsOuterVariable_ReportsError()
    {
        var code = """
func Main() {
    val b = 0
    if true {
        b
        val b = 1
    }
}
""";
        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult(CompilerDiagnostics.VariableUsedBeforeDeclaration.Id).WithSpan(4, 9, 4, 10).WithArguments("b"),
                new DiagnosticResult(CompilerDiagnostics.VariableShadowsPreviousDeclaration.Id)
                    .WithSpan(5, 13, 5, 14)
                    .WithArguments("b")
                    .WithSeverity(DiagnosticSeverity.Warning)
            ]);
        verifier.Verify();
    }
}
