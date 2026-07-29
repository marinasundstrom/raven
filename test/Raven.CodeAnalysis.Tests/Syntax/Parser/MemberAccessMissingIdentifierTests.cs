using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class MemberAccessMissingIdentifierTests : DiagnosticTestBase
{
    [Fact]
    public void MemberAccessWithoutIdentifier_ReportsDiagnostic()
    {
        const string code = """
        class C {
            func Test() -> unit {
                let a = 1
                let b = a.
            }
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult(CompilerDiagnostics.IdentifierExpected.Id).WithAnySpan()
        ]);
        verifier.Verify();
    }
}
