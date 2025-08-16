using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NamespaceResolutionTest : DiagnosticTestBase
{
    [Fact]
    public void NamespaceSystemDoesContainString_ShouldNot_ProduceDiagnostic()
    {
        string testCode =
            """
            System.String;
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void NamespaceSystemDoesNotContainFoo_Should_ProduceDiagnostic()
    {
        string testCode =
            """
            System.Foo;
            """;

        var verifier = CreateVerifier(
               testCode,
               [
                    new DiagnosticResult("RAV0234").WithLocation(1, 8).WithArguments("Foo", "System"),
               ]);

        verifier.Verify();
    }

    [Fact]
    public void ConsoleDoesContainWriteLine_ShouldNot_ProduceDiagnostics()
    {
        string testCode =
            """
            System.Console.WriteLine("Foo");
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();
    }

    [Fact]
    public void ConsoleDoesNotContainWriteLine2_Should_ProduceDiagnostics()
    {
        string testCode =
            """
            System.Console.WriteLine2("Foo");
            """;

        var verifier = CreateVerifier(
                    testCode,
                    [
                         new DiagnosticResult("RAV0117").WithLocation(1, 16).WithArguments("Console", "WriteLine2"),
                    ]);

        verifier.Verify();
    }

    [Fact]
    public void ConsoleDoesNotExistInCurrentContext_Should_ProduceDiagnostics()
    {
        string testCode =
            """
            Console.WriteLine2("Foo");
            """;

        var verifier = CreateVerifier(
                    testCode,
                    [
                         new DiagnosticResult("RAV0103").WithLocation(1, 1).WithArguments("Console"),
                    ]);

        verifier.Verify();
    }
}