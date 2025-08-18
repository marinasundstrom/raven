using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExpressionSemanticTest : DiagnosticTestBase
{
    [Fact]
    public void Foo_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            import System;

            Console.WriteLine(Console.WriteLine());
            """;

        var verifier = CreateVerifier(
                    testCode,
                    [
                         new DiagnosticResult("RAV1501").WithLocation(3, 1).WithArguments("WriteLine", "1")
                    ]);

        verifier.Verify();
    }
}
