using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class ExpressionBodyToBlockBodyCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void Method_ExpressionBodyWithValueReturn_ConvertsToReturnBlock()
    {
        const string code = """
class C {
    func Get() -> int => 42
}
""";

        const string fixedCode = """
class C {
    func Get() -> int {
        return 42
    }
}
""";

        var verifier = CreateCodeFixVerifier<ExpressionBodyToBlockBodyAnalyzer, ExpressionBodyToBlockBodyCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(ExpressionBodyToBlockBodyAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void Method_ExpressionBodyWithUnitReturn_ConvertsToExpressionStatementBlock()
    {
        const string code = """
class C {
    func Log() -> () => ()
}
""";

        const string fixedCode = """
class C {
    func Log() -> () {
        ()
    }
}
""";

        var verifier = CreateCodeFixVerifier<ExpressionBodyToBlockBodyAnalyzer, ExpressionBodyToBlockBodyCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(ExpressionBodyToBlockBodyAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
