using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class VarCanBeLetCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesBindingKeyword()
    {
        var code = "var count = 0";
        var fixedCode = "let count = 0";

        var verifier = CreateCodeFixVerifier<VarCanBeLetAnalyzer, VarCanBeLetCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(VarCanBeLetAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
