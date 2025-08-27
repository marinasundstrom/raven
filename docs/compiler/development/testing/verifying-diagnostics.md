# Verifying diagnostics

You can verify reported or missing diagnostics using the built-in testing framework.

Behind the scenes, a `Compilation` is created, from which the diagnostics is being fetched.

For analyzer scenarios, see the [Analyzer verifier](analyzer-verifier.md) which runs
analyzers through the workspace to capture diagnostics.

This example shows how to use the testing framework:

```csharp
using Raven.CodeAnalysis.Tests.Framework;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class DiagnosticVerifierTest
{
    [Fact]
    public void GetResult_WithIgnoredDiagnostics()
    {
        string testCode =
            """
            System.Console.WriteLine("Hello" + ", World!);
            """;

        var verifier = CreateVerifier(
            testCode,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1010").WithLocation(1, 36)
            ],
            disabledDiagnostics: [ "RAV1002" ]);

        var result = verifier.GetResult();

        result.MatchedDiagnostics.Count.ShouldBe(1);
        Assert.Empty(result.MissingDiagnostics);
        Assert.Empty(result.UnexpectedDiagnostics);
    }

    private DiagnosticVerifier CreateVerifier(string testCode, IEnumerable<DiagnosticResult>? expectedDiagnostics = null, IEnumerable<string>? disabledDiagnostics = null)
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
```