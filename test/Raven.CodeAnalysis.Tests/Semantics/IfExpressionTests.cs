using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IfExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void IfExpressionWithoutElse_ReportsDiagnostic()
    {
        const string code = """
val value = if true {
    42
}
""";

        var verifier = CreateVerifier(code,
            [new DiagnosticResult("RAV1901").WithAnySpan()]);

        verifier.Verify();
    }

    [Fact]
    public void IfExpressionWithElse_AllowsAssignment()
    {
        const string code = """
val value = if true {
    42
} else {
    0
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }
}
