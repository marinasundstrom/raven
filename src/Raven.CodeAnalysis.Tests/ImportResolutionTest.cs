using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class ImportResolutionTest : DiagnosticTestBase
{
    [Fact]
    public void NamespaceSystemNotImportedStringNotAvailable_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            String;
            """;

        var verifier = CreateVerifier(
                    testCode,
                    [
                         new DiagnosticResult("RAV0103").WithLocation(1, 1).WithArguments("String")
                    ]);

        verifier.Verify();
    }

    [Fact]
    public void ImportedNamespaceSystemDoesContainString_ShouldNot_ProduceDiagnostic()
    {
        string testCode =
            """
            import System;

            String;
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}