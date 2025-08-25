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
                M() -> void {}
            }

            Foo.M();
            """;

        var verifier = CreateVerifier(testCode,
            [new DiagnosticResult("RAV0117").WithLocation(5, 1).WithArguments("Foo", "M")]);

        verifier.Verify();
    }
}
