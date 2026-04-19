using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferTargetTypedUnionCaseCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void QualifiedUnionCaseConstruction_DoesNotApplyCodeFix()
    {
        var code = """
func Test() {
    val v = Option<int>.Some(0)
}

union Option<T> {
    case Some(value: T)
    case None
}
""";

        var verifier = CreateCodeFixVerifier<PreferTargetTypedUnionCaseAnalyzer, PreferTargetTypedUnionCaseCodeFixProvider>(
            code,
            code,
            [],
            expectedAppliedFixCount: 0,
            enableSuggestions: true);

        verifier.Verify();
    }
}
