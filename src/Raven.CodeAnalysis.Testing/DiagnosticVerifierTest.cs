using System;

using Shouldly;

namespace Raven.CodeAnalysis.Testing;

public class DiagnosticVerifierTest
{
    [Fact]
    public void GetResult_NoDiagnostics()
    {
        string testCode =
            """
            System.Console.WriteLine("Hello" + ", World!");
            """;

        var verifier = CreateVerifier(testCode);

        verifier.Verify();

        var result = verifier.GetResult();

        Assert.Empty(result.MatchedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    [Fact]
    public void Verify_ThrowsException_WhenMissingDiagnostic()
    {
        var exception = Assert.Throws<DiagnosticVerificationException>(() =>
        {
            string testCode =
                """
            System.Console.WriteLine("Hello" + ", World!");
            """;

            var verifier = CreateVerifier(
                 testCode,
                 [
                    new DiagnosticResult("RAV1002").WithSpan(1, 47, 1, 48)
                 ]);

            verifier.Verify();
        });

        var result = exception.DiagnosticResult;

        result.MatchedDiagnostics.Count.ShouldBe(0);
        result.MissingDiagnostics.Count.ShouldBe(1);
        result.UnexpectedDiagnostics.Count.ShouldBe(0);
        exception.Message.ShouldContain("(1,47 - 1,48)");
    }

    [Fact]
    public void GetResult_WithMatchedDiagnostics()
    {
        string testCode =
            """
            String
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("String"),
            ]);

        var result = verifier.GetResult();

        result.MatchedDiagnostics.Count.ShouldBe(1);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    [Fact]
    public void GetResult_WithIgnoredDiagnostics()
    {
        string testCode =
            """
            String
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("String")
            ],
            ["RAV1002"]);

        var result = verifier.GetResult();

        result.MatchedDiagnostics.Count.ShouldBe(1);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    [Fact]
    public void GetResult_WithSpan()
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

        var result = verifier.GetResult();

        result.MatchedDiagnostics.Count.ShouldBe(1);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    [Fact]
    public void GetResult_WithAnySpan()
    {
        string testCode =
            """
            String
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("String")
            ]);

        var result = verifier.GetResult();

        result.MatchedDiagnostics.Count.ShouldBe(1);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    private DiagnosticVerifier CreateVerifier(string testCode, IList<DiagnosticResult>? expectedDiagnostics = null, IList<string>? disabledDiagnostics = null)
    {
        return new DiagnosticVerifier
        {
            Test = new Test
            {
                TestCode = testCode,
                ExpectedDiagnostics = expectedDiagnostics ?? new List<DiagnosticResult>(),
                DisabledDiagnostics = disabledDiagnostics ?? new List<string>()
            }
        };
    }
}
