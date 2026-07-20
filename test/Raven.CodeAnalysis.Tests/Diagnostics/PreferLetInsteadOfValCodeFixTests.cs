using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferLetInsteadOfValCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesBindingKeyword()
    {
        var code = "val x = 1";
        var fixedCode = "let x = 1";

        var verifier = CreateCodeFixVerifier<PreferLetInsteadOfValAnalyzer, PreferLetInsteadOfValCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferLetInsteadOfValAnalyzer.PreferLetInsteadOfValDiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
