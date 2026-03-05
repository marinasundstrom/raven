using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class MemberAccessMissingIdentifierTests : DiagnosticTestBase
{
    [Fact]
    public void MemberAccessWithoutIdentifier_ReportsDiagnostic()
    {
        const string code = """
        class C {
            func Test() -> unit {
                val a = 1
                val b = a.
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.IdentifierExpected.Id).WithAnySpan()
        ]);
        verifier.Verify();
    }
}
