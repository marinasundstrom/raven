using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class MemberCanBeStaticCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_MakesMethodStatic()
    {
        const string code = """
class MathOps {
    func Add(x: int, y: int) -> int => x + y
}
""";

        const string fixedCode = """
class MathOps {
    static func Add(x: int, y: int) -> int => x + y
}
""";

        var verifier = CreateCodeFixVerifier<MemberCanBeStaticAnalyzer, MemberCanBeStaticCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(MemberCanBeStaticAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
