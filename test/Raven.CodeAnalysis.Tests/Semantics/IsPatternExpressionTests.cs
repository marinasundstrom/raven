using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IsPatternExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void IsPattern_WithIncompatibleLiteralPattern_ReportsDiagnostic()
    {
        const string code = """
record class Foo(Value: bool, Data: (int, int))

func Test(x: object?) {
    if x is Foo(_, true) {
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2102").WithAnySpan().WithArguments("true", "(int, int)")]);

        verifier.Verify();
    }
}
