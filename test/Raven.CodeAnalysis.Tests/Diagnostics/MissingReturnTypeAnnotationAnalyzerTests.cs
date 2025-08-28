using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MissingReturnTypeAnnotationAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void MethodWithoutAnnotation_SuggestsInferredReturnType()
    {
        const string code = """
class C {
    Test() {
        return 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId)
                    .WithSpan(2, 5, 2, 9)
                    .WithArguments("Test", "Unit")
            ],
            disabledDiagnostics: ["RAV1503"]);

        verifier.Verify();
    }

    [Fact]
    public void FunctionStatementWithoutAnnotation_SuggestsInferredReturnType()
    {
        const string code = """
func Test() {
    return 1
}
""";

        var verifier = CreateAnalyzerVerifier<MissingReturnTypeAnnotationAnalyzer>(code,
            expectedDiagnostics: [
                new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId)
                    .WithSpan(1, 6, 1, 10)
                    .WithArguments("Test", "Unit")
            ],
            disabledDiagnostics: ["RAV1503"]);

        verifier.Verify();
    }
}
