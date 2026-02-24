using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MemberCanBePrivateCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_MakesMethodPrivate()
    {
        const string code = """
class Counter {
    public func Increment() -> () { }
}
""";

        const string fixedCode = """
class Counter {
    private func Increment() -> () { }
}
""";

        var verifier = CreateCodeFixVerifier<MemberCanBePrivateAnalyzer, MemberCanBePrivateCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(MemberCanBePrivateAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
