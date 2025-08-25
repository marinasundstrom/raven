using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class PropertyDeclarationSyntaxTest : DiagnosticTestBase
{
    [Fact]
    public void StaticProperty()
    {
        string testCode =
            """
            class Foo {
                public static Value: int {
                    get => 0
                }
            }
            """;

        var verifier = CreateVerifier(testCode);
        verifier.Verify();
    }
}
