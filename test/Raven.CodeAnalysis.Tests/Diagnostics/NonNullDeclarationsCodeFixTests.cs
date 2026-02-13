using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class NonNullDeclarationsCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesNullableTypeToOption()
    {
        var code = """
func Test() {
    var value: int? = null
}
""";

        var fixedCode = """
func Test() {
    var value: Option<int> = null
}
""";

        var verifier = CreateCodeFixVerifier<NonNullDeclarationsAnalyzer, NonNullDeclarationsCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(NonNullDeclarationsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
