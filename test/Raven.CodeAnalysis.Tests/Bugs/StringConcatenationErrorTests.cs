using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class StringConcatenationErrorTests : DiagnosticTestBase
{
    [Fact]
    public void InterpolatedString_WithUndefinedExpression_ProducesDiagnostic()
    {
        const string code = """
        class Foo {
            Test() -> unit {
                let value = $"Value: {missing}"
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("missing")
        ]);

        verifier.Verify();
    }

    [Fact]
    public void StringConcatenation_WithUndefinedExpression_ProducesDiagnostic()
    {
        const string code = """
        class Foo {
            Test() -> unit {
                let value = "Value: " + missing
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("missing")
        ]);

        verifier.Verify();
    }
}
