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
                val value = $"Value: {missing}"
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("missing")
        ]);

        verifier.Verify();
    }

    [Fact]
    public void StringConcatenation_WithUndefinedExpression_ProducesDiagnostic()
    {
        const string code = """
        class Foo {
            Test() -> unit {
                val value = "Value: " + missing
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("missing")
        ]);

        verifier.Verify();
    }
}
