using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

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

    [Fact]
    public void NamespaceSystemNotImportedStringInClassNotAvailable_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            class C {
                let field: String;
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV0103").WithLocation(2, 16).WithArguments("String")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void GenericListWithCorrectImport_ShouldNot_ProduceDiagnostic()
    {
        string testCode =
            """
            import System.Collections.Generic;

            List<string>;
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void FullyQualifiedGenericListWithoutImport_ShouldNot_ProduceDiagnostic()
    {
        string testCode =
            """
            System.Collections.Generic.List<string>;
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }
}
