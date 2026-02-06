using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class StringLiteralTests : DiagnosticTestBase
{
    [Fact]
    public void StringLiteral_WithValidTermination_ShouldNotProduceDiagnostics()
    {
        string testCode =
            """
            "Hello, World!";
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void MultiLineStringLiteral_WithValidTermination_ShouldNotProduceDiagnostics()
    {
        string testCode = "\"\"\"\nHello\nWorld\n\"\"\";";

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void StringLiteral_WithMissingTermination_ShouldProduceExpectedDiagnostics()
    {
        string testCode =
            """
            "Hello, World!;
            """;

        var verifier = CreateVerifier(
               testCode,
               [
                    new DiagnosticResult("RAV1010").WithSpan(1, 1, 1, 1)
               ]);

        verifier.Verify();
    }
}
