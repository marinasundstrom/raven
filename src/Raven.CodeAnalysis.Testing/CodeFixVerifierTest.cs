using Raven.CodeAnalysis.Diagnostics;

using Shouldly;

namespace Raven.CodeAnalysis.Testing;

public class CodeFixVerifierTest
{
    [Fact]
    public void Verify_AppliesCodeFix_AndMatchesExpectedCode()
    {
        const string code = """
func Test() {
    return 1
}
""";

        const string fixedCode = """
func Test() -> int {
    return 1
}
""";

        var verifier = new CodeFixVerifier<MissingReturnTypeAnnotationAnalyzer, MissingReturnTypeAnnotationCodeFixProvider>
        {
            Test = new CodeFixTest
            {
                TestCode = code,
                FixedCode = fixedCode,
                ExpectedAppliedFixCount = 1,
                ExpectedDiagnostics =
                [
                    new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId).WithAnySpan()
                ],
                DisabledDiagnostics = ["RAV1011", "RAV1014", "RAV1503"],
                State = new TestState
                {
                    ReferenceAssemblies = ReferenceAssemblies.Default,
                }
            }
        };

        verifier.Verify();
    }

    [Fact]
    public void Verify_ThrowsException_WhenFixedCodeDoesNotMatch()
    {
        const string code = """
func Test() {
    return 1
}
""";

        var verifier = new CodeFixVerifier<MissingReturnTypeAnnotationAnalyzer, MissingReturnTypeAnnotationCodeFixProvider>
        {
            Test = new CodeFixTest
            {
                TestCode = code,
                FixedCode = "func Test() -> long { return 1 }",
                ExpectedAppliedFixCount = 1,
                ExpectedDiagnostics =
                [
                    new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId).WithAnySpan()
                ],
                DisabledDiagnostics = ["RAV1011", "RAV1014", "RAV1503"],
                State = new TestState
                {
                    ReferenceAssemblies = ReferenceAssemblies.Default,
                }
            }
        };

        var ex = Assert.Throws<CodeFixVerificationException>(() => verifier.Verify());
        ex.Message.ShouldContain("Expected fixed code:");
        ex.Message.ShouldContain("Actual fixed code:");
    }
}
