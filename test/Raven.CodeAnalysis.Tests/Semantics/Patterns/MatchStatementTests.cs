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
    func Eval(scrutinee: bool) -> bool {
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

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult("RAV1503").WithSpan(4, 21, 6, 14).WithArguments("()", "bool"),
                new DiagnosticResult("RAV1503").WithSpan(7, 22, 9, 14).WithArguments("()", "bool")
            ]);
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
    public void MatchStatement_WithOpenGenericDeclarationPattern_InfersTypeArgumentsFromScrutinee()
    {
        const string code = """
class Box<T> {}

val value: Box<int> = Box<int>()

match value {
    Box box => 1
}
""";

        var verifier = CreateVerifier(code);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);
        Assert.DoesNotContain(
            run.Compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var statement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var bound = Assert.IsType<BoundMatchStatement>(model.GetBoundNode(statement));

        var declaration = Assert.IsType<BoundDeclarationPattern>(bound.Arms[0].Pattern);
        var designator = Assert.IsType<BoundSingleVariableDesignator>(declaration.Designator);

        Assert.Equal("Box<int>", declaration.DeclaredType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        Assert.Equal("Box<int>", designator.Local.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void MatchStatement_WithNestedCaseNominalSequenceAndWholeDesignation_BindsWithoutDiagnostics()
    {
        const string code = """
union Option<T> {
    case Some(value: T)
    case None
}

class C {
    func Run(value: Option<(string, int)>) {
        match value {
            val Some((first, >= 18)) whole => {
                first.Length
                whole
            }
            _ => ()
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
