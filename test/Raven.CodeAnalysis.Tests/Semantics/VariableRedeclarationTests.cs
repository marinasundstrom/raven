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
let x : 'a' | 1 = 1
let x : 'a' | 1 = 'a'
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
func main() {
    let x : 'a' | 1 = 1
    let x : 'a' | 1 = 'a'
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
let x : 'a' | 1 = 1
{
    let x : 'a' | 1 = 'a'
}
""";
            var verifier = CreateVerifier(code,
                expectedDiagnostics: [
                    new DiagnosticResult("RAV0168").WithSpan(3, 9, 3, 10).WithArguments("x").WithSeverity(DiagnosticSeverity.Warning)
                ]);
            verifier.Verify();
        }
    }
