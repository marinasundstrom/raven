using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class MemberAccessMissingIdentifierTests : DiagnosticTestBase
{
    [Fact]
    public void MemberAccessWithoutIdentifier_ReportsDiagnostic()
    {
        const string code = """
        class C {
            Test() -> unit {
                let a = 1
                let b = a.
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1001").WithAnySpan(),
            new DiagnosticResult("RAV0103").WithAnySpan().WithArguments("")
        ]);
        verifier.Verify();
    }
}
