using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class ResultExtensionPropertyResolutionTests : DiagnosticTestBase
{
    [Fact]
    public void ResultIsErrorBindsToSingleTypeResultExtension()
    {
        string testCode =
            """
            import System.*

            val res: Result<int, string> = .Error("Bang")

            Console.WriteLine("Is error: ${res.HasError}")
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
