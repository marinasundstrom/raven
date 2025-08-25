using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AliasResolutionTest : DiagnosticTestBase
{
    [Fact]
    public void AliasDirective_UsesAlias()
    {
        string testCode =
            """
            alias SB = System.Text.StringBuilder

            SB
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesAlias_Generic()
    {
        string testCode =
            """
            alias IntList = System.Collections.Generic.List<int>

            IntList
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesAlias_AsTypeAnnotation()
    {
        string testCode =
            """
            alias StringList = System.Collections.Generic.List<string>

            let list: StringList
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void AliasDirective_UsesMemberAlias_Method()
    {
        string testCode =
            """
            alias PrintLine = System.Console.WriteLine

            PrintLine(123)
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

}
