using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MissingReturnTypeAnnotationCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_AddsArrowTypeClause()
    {
        var code = """
func Test() {
    return 1
}
""";

        var fixedCode = """
func Test() -> int {
    return 1
}
""";

        var verifier = CreateCodeFixVerifier<MissingReturnTypeAnnotationAnalyzer, MissingReturnTypeAnnotationCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(MissingReturnTypeAnnotationAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
