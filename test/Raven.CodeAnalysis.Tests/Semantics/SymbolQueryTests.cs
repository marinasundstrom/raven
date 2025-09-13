using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SymbolQueryTests : DiagnosticTestBase
{
    [Fact]
    public void CallingInstanceMethodAsStatic_ProducesDiagnostic()
    {
        string testCode =
            """
            class Foo {
                M() {}
            }

            Foo.M();
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0117").WithSpan(5, 5, 5, 6).WithArguments("Foo", "M")]);

        verifier.Verify();
    }
}
