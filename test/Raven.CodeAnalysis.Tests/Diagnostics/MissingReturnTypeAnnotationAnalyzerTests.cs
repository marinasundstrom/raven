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
                    .WithLocation(2, 5)
                    .WithArguments("Test", "Unit")
            ],
            disabledDiagnostics: ["RAV1503"]);

        verifier.Verify();
    }
}
