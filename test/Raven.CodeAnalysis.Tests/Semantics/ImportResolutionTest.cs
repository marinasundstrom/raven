using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ImportResolutionTest : DiagnosticTestBase
{
    [Fact]
    public void NamespaceSystemNotImportedStringNotAvailable_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            String
            """;

        var verifier = CreateVerifier(
                    testCode,
                    [
                         new DiagnosticResult("RAV0103").WithSpan(1, 1, 1, 7).WithArguments("String")
                    ]);

        verifier.Verify();
    }

    [Fact]
    public void ImportedNamespaceSystemDoesContainString_ShouldNot_ProduceDiagnostic()
    {
        string testCode =
            """
            import System.*

            String
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
                let field: String
            }
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV0103").WithSpan(2, 16, 2, 22).WithArguments("String")
            ]);

        verifier.Verify();
    }

    [Fact]
    public void GenericListWithCorrectImport_ShouldNot_ProduceDiagnostic()
    {
        string testCode =
            """
            import System.Collections.Generic.*

            List<string>
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void FullyQualifiedGenericListWithoutImport_ShouldNot_ProduceDiagnostic()
    {
        string testCode =
            """
            System.Collections.Generic.List<string>
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ImportFullyQualifiedOpenGenericType_ShouldNot_ProduceDiagnostic2()
    {
        string testCode =
            """
            import System.Collections.Generic.List<>

            List<int>
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void OpenGenericTypeWithoutTypeArguments_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            import System.Collections.Generic.*

            var x: List = null
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV0305").WithSpan(3, 8, 3, 12).WithArguments("List", 1)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void SpecificTypeImport_MakesTypeAvailable()
    {
        string testCode =
            """
            import System.Text.StringBuilder

            StringBuilder
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void WildcardImport_MakesAllTypesAvailable()
    {
        string testCode =
            """
            import System.Text.*

            StringBuilder
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void WildcardImport_MakesSystemMathMembersAvailable()
    {
        string testCode =
            """
            import System.Math.*

            let pi = PI
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void NamespaceImportWithoutWildcard_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            import System

            String
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV0235").WithSpan(1, 8, 1, 14),
                new DiagnosticResult("RAV0103").WithSpan(3, 1, 3, 7).WithArguments("String"),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void WildcardTypeImport_MakesStaticMembersAvailable()
    {
        string testCode =
            """
            import System.Console.*

            WriteLine("test")
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ImportNonNamespaceOrType_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            import System.Console.WriteLine.*
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV2021").WithSpan(1, 8, 1, 32),
            ]);

        verifier.Verify();
    }
}
