using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferNewLineBetweenDeclarationsCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_InsertsNewLine()
    {
        var code = "class A {};trait T for A {}";
        var fixedCode = """
class A {};
trait T for A {}
""";

        var verifier = CreateCodeFixVerifier<PreferNewLineBetweenDeclarationsAnalyzer, PreferNewLineBetweenDeclarationsCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferNewLineBetweenDeclarationsAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
