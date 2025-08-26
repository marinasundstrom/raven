using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExpressionSemanticTest : DiagnosticTestBase
{
    [Fact]
    public void NestedWriteLine_WithUnitArgument_IsAllowed()
    {
        string testCode =
            """
            import System.*

            Console.WriteLine(Console.WriteLine("Test"));
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
