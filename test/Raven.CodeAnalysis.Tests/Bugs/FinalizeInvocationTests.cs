using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public sealed class FinalizeInvocationTests : DiagnosticTestBase
{
    [Fact]
    public void Finalize_IsNotInvocableViaMemberAccess()
    {
        const string code = """
        import System.*

        func Test(value: string) -> unit {
            value.Finalize()
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.MemberDoesNotContainDefinition.Id)
                .WithSpan(4, 11, 4, 19)
                .WithArguments("String", "Finalize")
        ]);

        verifier.Verify();
    }
}
