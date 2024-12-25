using System;

using Raven.CodeAnalysis.Tests.Framework;

using Shouldly;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class StringLiteralTests : DiagnosticTestBase
{
    [Fact]
    public void StringLiteral_WithValidTermination_ShouldNotProduceDiagnostics()
    {
        string testCode =
            """
            "Hello, World!";
            """;

        var verifier = CreateVerifier(testCode);

        var result = verifier.GetResult();

        Assert.Empty(result.MatchedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    [Fact]
    public void StringLiteral_WithMissingTermination_ShouldProduceExpectedDiagnostics()
    {
        string testCode =
            """
            "Hello, World!;
            """;

        var verifier = CreateVerifier(
               testCode,
               [
                    new DiagnosticResult("RAV1010").WithLocation(1, 1),
                    new DiagnosticResult("RAV1002").WithLocation(1, 16)
               ]);

        var result = verifier.GetResult();

        result.MatchedDiagnostics.Count.ShouldBe(2);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }
}