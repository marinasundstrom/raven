using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MatchStatementTests : DiagnosticTestBase
{
    [Fact]
    public void MatchStatement_PrefixForm_BindsLikeMatchExpression()
    {
        const string code = """
match 1 {
    1 => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_PrefixForm_MissingCoverageReportsExhaustivenessDiagnostic()
    {
        const string code = """
match 1 {
    1 => 1
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV2100").WithAnySpan().WithArguments("_")]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_PrefixFormWithBlockArms_AfterPreviousStatement_BindsWithoutDiagnostics()
    {
        const string code = """
val value: bool = true

match value {
    true => { 1 }
    false => { 0 }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_PrefixFormWithReturnInArmBlocks_AllowsReturnStatements()
    {
        const string code = """
class Evaluator {
    Eval(scrutinee: bool) -> bool {
        match scrutinee {
            true => {
                return true
            }
            false => {
                return false
            }
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
