using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class UninitializedLocalTests : DiagnosticTestBase
{
    [Fact]
    public void LocalDeclarationWithoutInitializer_NoDiagnostics()
    {
        const string code = """
        class Foo {
            Test() -> unit {
                var x: int;
            }
        }
        """;

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
