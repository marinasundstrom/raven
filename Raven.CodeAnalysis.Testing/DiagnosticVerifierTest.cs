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
                    new DiagnosticResult("RAV1002").WithLocation(1, 47)
                 ]);

            verifier.Verify();
        });

        var result = exception.DiagnosticResult;

        result.MatchedDiagnostics.Count.ShouldBe(0);
        result.MissingDiagnostics.Count.ShouldBe(1);
        result.UnexpectedDiagnostics.Count.ShouldBe(0);
    }

    [Fact]
    public void GetResult_WithMatchedDiagnostics()
    {
        string testCode =
            """
            System.Console.WriteLine("Hello" + ", World!);
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV1010").WithLocation(1, 36),
                new DiagnosticResult("RAV1002").WithLocation(1, 47)
            ]);

        var result = verifier.GetResult();

        result.MatchedDiagnostics.Count.ShouldBe(2);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    [Fact]
    public void GetResult_WithIgnoredDiagnostics()
    {
        string testCode =
            """
            System.Console.WriteLine("Hello" + ", World!);
            """;

        var verifier = CreateVerifier(
            testCode,
            [
                new DiagnosticResult("RAV1010").WithLocation(1, 36)
            ],
            ["RAV1002"]);

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