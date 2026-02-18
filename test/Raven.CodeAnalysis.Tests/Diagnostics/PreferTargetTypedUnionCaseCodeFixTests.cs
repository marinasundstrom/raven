using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferTargetTypedUnionCaseCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesDeclaration()
    {
        var code = """
func Test() {
    val v = Option<int>.Some(0)
}

union Option<T> {
    Some(value: T)
    None
}
""";

        var fixedCode = """
func Test() {
    val v: Option<int> = .Some(0)
}

union Option<T> {
    Some(value: T)
    None
}
""";

        var verifier = CreateCodeFixVerifier<PreferTargetTypedUnionCaseAnalyzer, PreferTargetTypedUnionCaseCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferTargetTypedUnionCaseAnalyzer.DiagnosticId).WithAnySpan()],
            enableSuggestions: true);

        verifier.Verify();
    }
}
