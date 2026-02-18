using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class IfStatementValueWarningTests : DiagnosticTestBase
{
    [Fact]
    public void IfStatement_NotLastInReturningMethod_ValueBranchesReportIgnoredValueWarning()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    if flag {
        1
    } else {
        0
    }

    return 42
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2108").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithExplicitReturnBranches_DoesNotReportIgnoredValueWarning()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    if flag {
        return 1
    } else {
        return 0
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
