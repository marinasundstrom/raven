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
                val x: string = y;
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.TheNameDoesNotExistInTheCurrentContext.Id).WithAnySpan().WithArguments("y")
        ]);
        verifier.Verify();
    }
}
