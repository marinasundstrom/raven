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

        verifier.Verify();
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

        verifier.Verify();
    }
}