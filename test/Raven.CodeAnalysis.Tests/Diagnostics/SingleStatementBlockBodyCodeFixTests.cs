using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class SingleStatementBlockBodyCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void Method_ReturnBlock_ConvertsToExpressionBody()
    {
        const string code = """
class C {
    func Get() -> int {
        return 42
    }
}
""";

        const string fixedCode = """
class C {
    func Get() -> int => 42
}
""";

        var verifier = CreateCodeFixVerifier<SingleStatementBlockBodyAnalyzer, SingleStatementBlockBodyCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(SingleStatementBlockBodyAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void Function_ReturnBlock_ConvertsToExpressionBody()
    {
        const string code = """
func Get() -> int {
    return 42
}
""";

        const string fixedCode = """
func Get() -> int => 42
""";

        var verifier = CreateCodeFixVerifier<SingleStatementBlockBodyAnalyzer, SingleStatementBlockBodyCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(SingleStatementBlockBodyAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
