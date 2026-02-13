using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class PreferTargetTypedUnionCaseInTargetTypedContextCodeFixTests : CodeFixTestBase
{
    [Fact]
    public void AppliesCodeFix_RewritesInvocationArgument()
    {
        var code = """
func Foo(x: Shape) {}

func Test() {
    Foo(Shape.Circle(2))
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var fixedCode = """
func Foo(x: Shape) {}

func Test() {
    Foo(.Circle(2))
}

union Shape {
    Circle(radius: int)
    Rectangle(width: int, height: int)
}
""";

        var verifier = CreateCodeFixVerifier<PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer, PreferTargetTypedUnionCaseInTargetTypedContextCodeFixProvider>(
            code,
            fixedCode,
            [new DiagnosticResult(PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer.DiagnosticId).WithAnySpan()]);

        verifier.Verify();
    }
}
