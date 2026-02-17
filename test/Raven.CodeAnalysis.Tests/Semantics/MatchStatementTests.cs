using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MatchStatementTests : DiagnosticTestBase
{
    [Fact]
    public void MatchStatement_PrefixForm_BindsAsBoundMatchStatement()
    {
        const string code = """
match 1 {
    1 => 1
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_bound_shape",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var statement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var bound = model.GetBoundNode(statement);

        var matchStatement = Assert.IsType<BoundMatchStatement>(bound);
        Assert.Equal(2, matchStatement.Arms.Length);
    }

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

    [Fact]
    public void MatchStatement_ImplicitReturn_LastInBlock_TargetTypesArmMemberBindings()
    {
        const string code = """
enum PingStatus {
    Ok,
    Error
}

func ping(name: string) -> PingStatus {
    match name {
        "Bob" | "bob" => .Ok
        _ => .Error
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_NotLastInBlock_TargetTypesArmMemberBindings_AndWarnsValueIgnored()
    {
        const string code = """
enum PingStatus {
    Ok,
    Error
}

func ping(name: string) -> PingStatus {
    match name {
        "Bob" | "bob" => .Ok
        _ => .Error
    }

    return PingStatus.Error
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2107").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_NotLastInReturningMethod_ValueArmsReportIgnoredValueWarning()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    match flag {
        true => 1
        false => 0
    }

    return 42
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV2107").WithAnySpan(),
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MatchStatement_LastInReturningMethod_DoesNotReportIgnoredValueWarning()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    match flag {
        true => 1
        false => 0
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
