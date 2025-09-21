using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class FieldDeclarationMissingLetOrVarTests : DiagnosticTestBase
{
    [Fact]
    public void FieldWithoutLetOrVar_ReportsDiagnostic()
    {
        const string code = """
        class Foo {
            name: string = ""
        }
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV1007").WithAnySpan(),
        ]);

        verifier.Verify();
    }
}
