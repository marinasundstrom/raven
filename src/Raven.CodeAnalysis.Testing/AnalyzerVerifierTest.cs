using Raven.CodeAnalysis.Diagnostics;
using Shouldly;

namespace Raven.CodeAnalysis.Testing;

public class AnalyzerVerifierTest
{
    [Fact]
    public void Verify_ThrowsException_WhenMissingDiagnostic()
    {
        const string code = "class C { }";

        var verifier = new AnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>
        {
            Test = new Test
            {
                TestCode = code,
                ExpectedDiagnostics =
                [
                    new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId).WithSpan(1, 1, 1, 1)
                ]
            }
        };

        var exception = Assert.Throws<DiagnosticVerificationException>(() => verifier.Verify());
        exception.Message.ShouldContain("(1,1 - 1,1)");
    }
}
