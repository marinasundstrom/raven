using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TryStatementValueWarningTests : DiagnosticTestBase
{
    [Fact]
    public void TryStatement_NotLastInReturningMethod_ValueBlocksReportIgnoredValueWarning()
    {
        const string code = """
import System.*

func evaluate() -> int {
    try {
        1
    } catch (Exception ex) {
        0
    }

    return 42
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2109").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void TryStatement_WithExplicitReturnBlocks_DoesNotReportIgnoredValueWarning()
    {
        const string code = """
import System.*

func evaluate() -> int {
    try {
        return 1
    } catch (Exception ex) {
        return 0
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
