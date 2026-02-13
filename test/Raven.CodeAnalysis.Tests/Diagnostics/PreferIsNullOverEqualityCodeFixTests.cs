using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferIsNullOverEqualityCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesToIsNotNull()
    {
        var code = """
class C
{
    Run(x: int?) -> unit
    {
        if x != null { }
    }
}
""";

        var fixedCode = """
class C
{
    Run(x: int?) -> unit
    {
        if x is not null { }
    }
}
""";

        var verifier = CreateCodeFixVerifier<PreferIsNullOverEqualityAnalyzer, PreferIsNullOverEqualityCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferIsNullOverEqualityAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
