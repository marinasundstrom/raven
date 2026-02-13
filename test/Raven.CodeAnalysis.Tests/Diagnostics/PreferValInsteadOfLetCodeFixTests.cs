using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferValInsteadOfLetCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesBindingKeyword()
    {
        var code = "let x = 1";
        var fixedCode = "val x = 1";

        var verifier = CreateCodeFixVerifier<PreferValInsteadOfLetAnalyzer, PreferValInsteadOfLetCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferValInsteadOfLetAnalyzer.PreferValInsteadOfLetDiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
