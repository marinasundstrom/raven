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
                val a = 1
                val b = a.
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
