using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
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
    public void MatchStatement_WithOuterValNamedTypedTargetWithoutInlineBinding_ReportsDiagnostic()
    {
        const string code = """
record class Person(Name: string, Age: int)

val person = Person("Ada", 42)

match person {
    val (Name: name: string, Age: age: int) => 1
    _ => 0
}
""";

        var verifier = CreateVerifier(
            code,
            [
                new DiagnosticResult(CompilerDiagnostics.PatternTypedBindingRequiresKeyword.Id)
                    .WithAnySpan()
                    .WithArguments("name", "string"),
                new DiagnosticResult(CompilerDiagnostics.PatternTypedBindingRequiresKeyword.Id)
                    .WithAnySpan()
                    .WithArguments("age", "int")
            ]);

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
    public void MatchStatement_WithStructUnionDefaultLocal_AllCasesCoveredIsSourceExhaustive()
    {
        const string code = """
union State {
    case On
    case Off
}

val state: State = default

match state {
    .On => 1
    .Off => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_struct_union_default_exhaustiveness",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var statement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var info = model.GetMatchExhaustiveness(statement);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
    }

    [Fact]
    public void MatchStatement_WithFiniteTupleRows_IsExhaustiveInDiagnosticsAndSemanticModel()
    {
        const string code = """
val pair: (bool, bool) = (true, false)

match pair {
    (true, _) => 1
    (false, true) => 2
    (false, false) => 3
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_finite_tuple_exhaustiveness",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.MatchExpressionNotExhaustive);

        var model = compilation.GetSemanticModel(tree);
        var statement = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var info = model.GetMatchExhaustiveness(statement);

        Assert.True(info.IsExhaustive);
        Assert.Empty(info.MissingCases);
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
    public void MatchStatement_WithRangeAndTypedPositionalArms_BindsPatternsAndLocals()
    {
        const string code = """
record SupportTicket(Id: int, Priority: int)

func Route(ticket: SupportTicket, target: object) -> unit {
    match ticket.Priority {
        1 => ()
        2..3 => ()
        _ => ()
    }

    match target {
        SupportTicket(val id, val priority) => ()
        string teamCode => ()
        _ => ()
    }
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();

        Assert.Empty(result.UnexpectedDiagnostics);
        Assert.Empty(result.MissingDiagnostics);

        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var statements = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().ToArray();

        Assert.Equal(2, statements.Length);

        var priorityMatch = Assert.IsType<BoundMatchStatement>(model.GetBoundNode(statements[0]));
        Assert.IsType<BoundRangePattern>(priorityMatch.Arms[1].Pattern);

        var targetMatch = Assert.IsType<BoundMatchStatement>(model.GetBoundNode(statements[1]));
        var ticketPattern = Assert.IsType<BoundDeconstructPattern>(targetMatch.Arms[0].Pattern);
        Assert.Equal("SupportTicket", ticketPattern.ReceiverType.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));

        var typedPattern = Assert.IsType<BoundDeclarationPattern>(targetMatch.Arms[1].Pattern);
        Assert.Equal(SpecialType.System_String, typedPattern.Type.SpecialType);

        var locals = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .ToDictionary(
                static designation => designation.Identifier.ValueText,
                designation => Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(designation)));

        Assert.Equal(SpecialType.System_Int32, locals["id"].Type.SpecialType);
        Assert.Equal(SpecialType.System_Int32, locals["priority"].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, locals["teamCode"].Type.SpecialType);
    }

    [Fact]
    public void MatchStatement_WithNestedCaseNominalSequenceAndWholeDesignation_BindsWithoutDiagnostics()
    {
        const string code = """
import Option.*

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
