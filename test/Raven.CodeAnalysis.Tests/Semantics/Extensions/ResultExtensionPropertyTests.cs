using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ResultExtensionPropertyTests : DiagnosticTestBase
{
    [Fact]
    public void ResultIsErrorBindsToSingleTypeResultExtension()
    {
        string testCode =
            """
            import System.*

            union Result<T, E> {
                Ok(value: T)
                Error(error: E)
            }

            extension ResultExtensions<T, E> for Result<T, E> {
                val HasError: bool => self is Error(_)
            }

            val res: Result<int, string> = Error("Bang")

            Console.WriteLine("Is error: ${res.HasError}")
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
