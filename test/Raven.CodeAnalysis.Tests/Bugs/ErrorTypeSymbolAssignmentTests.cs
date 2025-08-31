using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class ErrorTypeSymbolAssignmentTests : DiagnosticTestBase
{
    [Fact]
    public void UndefinedIdentifierAssignedToExplicitType_DoesNotProduceAssignmentDiagnostic()
    {
        const string code = """
        class Foo {
            Test() -> unit {
                let x: string = y;
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("y")
        ]);
        verifier.Verify();
    }
}
