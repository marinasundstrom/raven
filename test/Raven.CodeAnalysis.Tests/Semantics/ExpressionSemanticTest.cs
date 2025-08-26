using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExpressionSemanticTest : DiagnosticTestBase
{
    [Fact]
    public void NestedWriteLine_WithUnitArgument_ProducesDiagnostic()
    {
        string testCode =
            """
            import System.*

            Console.WriteLine(Console.WriteLine("Test"));
            """;

        var verifier = CreateVerifier(
                    testCode,
                    [
                         new DiagnosticResult("RAV1501").WithLocation(3, 1).WithArguments("WriteLine", "1")
                    ]);

        verifier.Verify();
    }
}
