using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ExpressionSemanticTest : DiagnosticTestBase
{
    [Fact]
    public void NestedWriteLine_WithUnitArgument_ShouldNot_ProduceDiagnostics()
    {
        string testCode =
            """
            import System.*

            Console.WriteLine(Console.WriteLine())
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void WriteLine_WithUnitVariable_ShouldNot_ProduceDiagnostics()
    {
        string testCode =
            """
            import System.*

            let test = Console.WriteLine("Hello")
            Console.WriteLine(test)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
