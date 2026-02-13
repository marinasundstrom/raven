using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class VarCanBeValCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesBindingKeyword()
    {
        var code = "var count = 0";
        var fixedCode = "val count = 0";

        var verifier = CreateCodeFixVerifier<VarCanBeValAnalyzer, VarCanBeValCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(VarCanBeValAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
