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
val x : 'a' | 1 = 1
val x : 'a' | 1 = 'a'
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
    val x : 'a' | 1 = 1
    val x : 'a' | 1 = 'a'
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
val x : 'a' | 1 = 1
{
    val x : 'a' | 1 = 'a'
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
}
