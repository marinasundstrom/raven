using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class AssignmentAnalysisTests : CompilationTestBase
{
    [Fact]
    public void AnalyzeDataFlow_AssignmentStatement_WritesLocal()
    {
        const string source = """
var value = 0
value = 1
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        var analysis = model.AnalyzeDataFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.WrittenInside.ShouldContain(symbol => symbol.Name == "value");
        analysis.DataFlowsOut.ShouldContain(symbol => symbol.Name == "value");
    }

    [Fact]
    public void AnalyzeDataFlow_PatternAssignment_WritesEachLocal()
    {
        const string source = """
var first = 0
var second = 0
(first, second) = (1, 2)
first + second
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        var boundNode = model.GetBoundNode(assignment);
        var patternAssignment = boundNode switch
        {
            BoundAssignmentStatement assignmentStatement => assignmentStatement.Expression,
            BoundExpressionStatement expressionStatement => expressionStatement.Expression,
            _ => null,
        };

        patternAssignment.ShouldNotBeNull();
        (patternAssignment is BoundPatternAssignmentExpression or BoundErrorExpression).ShouldBeTrue();

        var analysis = model.AnalyzeDataFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.WrittenInside.Select(symbol => symbol.Name)
            .ShouldBe(new[] { "first", "second" }, ignoreOrder: true);
        var dataFlowsOutNames = analysis.DataFlowsOut.Select(symbol => symbol.Name).ToArray();
        dataFlowsOutNames.ShouldContain("first");
        dataFlowsOutNames.ShouldContain("second");
    }

    [Fact]
    public void AnalyzeDataFlow_DiscardAssignment_IgnoresWrites()
    {
        const string source = "_ = 1";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        var analysis = model.AnalyzeDataFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.WrittenInside.ShouldBeEmpty();
        analysis.DataFlowsOut.ShouldBeEmpty();
    }

    [Fact]
    public void AnalyzeControlFlow_AssignmentStatement_Succeeds()
    {
        const string source = """
var value = 0
value = 1
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Last();

        var analysis = model.AnalyzeControlFlow(assignment);

        analysis.Succeeded.ShouldBeTrue();
        analysis.StartPointIsReachable.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeTrue();
        analysis.ReturnStatements.ShouldBeEmpty();
    }

    [Fact]
    public void AnalyzeControlFlow_ReturnFollowedByStatement_MarksUnreachable()
    {
        const string source = """
func Main() {
    return;
    var value = 1;
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;

        var analysis = model.AnalyzeControlFlow(block);

        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
    }

    [Fact]
    public void AnalyzeControlFlow_IfStatementWithSingleStatementBody_TracksReturn()
    {
        const string source = """
func Main(flag: bool) -> int {
    if flag
        return 1

    return 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var ifStatement = tree.GetRoot()
            .DescendantNodes()
            .OfType<IfStatementSyntax>()
            .Single();

        var analysis = model.AnalyzeControlFlow(ifStatement);

        analysis.Succeeded.ShouldBeTrue();
        analysis.StartPointIsReachable.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeTrue();
        var returnStatement = analysis.ReturnStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<ReturnStatementSyntax>();
        returnStatement.Expression.ShouldBeOfType<LiteralExpressionSyntax>();
    }

    [Fact]
    public void GetDiagnostics_UnreachableStatement_ProducesWarning()
    {
        const string source = """
func Main() {
    return;
    var value = 1;
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetSemanticModel(tree).GetDiagnostics();

        var unreachable = diagnostics.Single(d => d.Descriptor == CompilerDiagnostics.UnreachableCodeDetected);
        unreachable.Severity.ShouldBe(DiagnosticSeverity.Warning);
    }

    [Fact]
    public void GetDiagnostics_FinallyAfterReturn_IsReachable()
    {
        const string source = """
func Main() -> int {
    try {
        return 1
    }
    finally {
        let cleanup = 0
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetSemanticModel(tree).GetDiagnostics();

        diagnostics.ShouldNotContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnreachableCodeDetected);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_AbruptFinallyReplacesContinueAndMakesFollowingCodeUnreachable(bool diagnosticsFirst)
    {
        const string source = """
func Main() {
    loop {
        try {
            continue
        } finally {
            return
        }
    }

    let unreachable = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);
        var diagnostics = compilation.GetDiagnostics();

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
        diagnostics.ShouldContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnreachableCodeDetected);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_FinallyContinueReplacesBreakAndKeepsLoopNonCompleting(bool diagnosticsFirst)
    {
        const string source = """
func Main() {
    loop {
        try {
            break
        } finally {
            continue
        }
    }

    let unreachable = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);
        var diagnostics = compilation.GetDiagnostics();

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
        diagnostics.ShouldContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.UnreachableCodeDetected);
    }

    [Fact]
    public void AnalyzeControlFlow_NonTerminatingLoop_MakesFollowingStatementUnreachable()
    {
        const string source = """
func Main() {
    loop {
    }

    let unreachable = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;

        var analysis = model.AnalyzeControlFlow(block);

        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
    }

    [Fact]
    public void AnalyzeControlFlow_ConstantTrueWhileLoop_MakesFollowingStatementUnreachable()
    {
        const string source = """
func Main() {
    while true {
    }

    let unreachable = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;

        var analysis = model.AnalyzeControlFlow(block);

        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_ConstantFalseCatchFilterHasNoNormalSuccessor(bool diagnosticsFirst)
    {
        const string source = """
import System.*

func Compute() -> int {
    try {
        return 1
    } catch Exception error when false {
        let ignored = 0
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_AbruptIfExpressionInitializer_MakesBlockEndUnreachable(bool diagnosticsFirst)
    {
        const string source = """
func Compute(flag: bool) -> int {
    let never = if flag {
        return 1
    } else {
        return 2
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            compilation.GetDiagnostics().ShouldBeEmpty();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
    }

    [Theory]
    [InlineData("Identity(return 1)", false)]
    [InlineData("Identity(return 1)", true)]
    [InlineData("-(return 1)", false)]
    [InlineData("-(return 1)", true)]
    [InlineData("(return 1).ToString()", false)]
    [InlineData("(return 1).ToString()", true)]
    public void AnalyzeControlFlow_NestedReturnExpressionMakesFollowingCodeUnreachable(
        string abruptExpression,
        bool diagnosticsFirst)
    {
        var source = $$"""
func Identity(value: int) -> int => value

func Compute() -> int {
    let never = {{abruptExpression}}
    let unreachable = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(function => function.Identifier.ValueText == "Compute")
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.ReturnStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<ReturnExpressionSyntax>();
        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
        compilation.GetDiagnostics().ShouldNotContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NotAllCodePathsReturnAValue);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_NestedReturnInIfConditionMakesBranchesAndFollowingCodeUnreachable(
        bool diagnosticsFirst)
    {
        const string source = """
func Identity(value: int) -> int => value

func Compute() -> int {
    if Identity(return 1) == 0 {
        let unreachableThen = 0
    } else {
        let unreachableElse = 0
    }

    let unreachableAfter = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(function => function.Identifier.ValueText == "Compute")
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.ReturnStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<ReturnExpressionSyntax>();
        analysis.UnreachableStatements.Count().ShouldBe(3);
        compilation.GetDiagnostics().ShouldNotContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NotAllCodePathsReturnAValue);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_NestedReturnInMatchScrutineeMakesArmsAndFollowingCodeUnreachable(
        bool diagnosticsFirst)
    {
        const string source = """
func Identity(value: int) -> int => value

func Compute() -> int {
    match Identity(return 1) {
        _ => 0
    }

    let unreachableAfter = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(function => function.Identifier.ValueText == "Compute")
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);

        analysis.Succeeded.ShouldBeTrue();
        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.ReturnStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<ReturnExpressionSyntax>();
        analysis.UnreachableStatements.ShouldNotBeEmpty();
        compilation.GetDiagnostics().ShouldNotContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NotAllCodePathsReturnAValue);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_NestedReturnInCatchFilterMakesCatchAndFollowingCodeUnreachable(
        bool diagnosticsFirst)
    {
        const string source = """
func Compute() -> int {
    try {
        throw Exception()
    } catch Exception error when (return 1) == 0 {
        let unreachableCatch = 0
    }

    let unreachableAfter = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var body = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(function => function.Identifier.ValueText == "Compute")
            .Body!;
        var analysis = compilation.GetSemanticModel(tree).AnalyzeControlFlow(body);

        analysis.Succeeded.ShouldBeTrue();
        analysis.ReturnStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<ReturnExpressionSyntax>();
        analysis.UnreachableStatements.Count().ShouldBe(2);
        analysis.EndPointIsReachable.ShouldBeFalse();
        compilation.GetDiagnostics().ShouldNotContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
        compilation.GetDiagnostics().ShouldNotContain(
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NotAllCodePathsReturnAValue);
    }

    [Fact]
    public void AnalyzeControlFlow_ExhaustiveAbruptMatch_MakesFollowingStatementUnreachable()
    {
        const string source = """
func Compute(flag: bool) -> int {
    match flag {
        true => return 1
        false => return 0
    }

    let unreachable = 0
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;

        var analysis = model.AnalyzeControlFlow(block);

        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.ReturnStatements.Count().ShouldBe(2);
        analysis.ReturnStatements.ShouldAllBe(static statement => statement is ReturnExpressionSyntax);
        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void AnalyzeControlFlow_ExhaustiveNullableAbruptMatch_MakesFollowingStatementUnreachable(bool diagnosticsFirst)
    {
        const string source = """
            func Compute(input: string?) -> int {
                match input {
                    string text => return text.Length
                    null => return 0
                }

                let unreachable = 0
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();

        var model = compilation.GetSemanticModel(tree);
        var block = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single()
            .Body!;
        var match = block.DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var analysis = model.AnalyzeControlFlow(block);
        var exhaustiveness = model.GetMatchExhaustiveness(match);

        analysis.EndPointIsReachable.ShouldBeFalse();
        analysis.ReturnStatements.Count().ShouldBe(2);
        analysis.UnreachableStatements.ShouldHaveSingleItem()
            .ShouldBeOfType<LocalDeclarationStatementSyntax>();
        exhaustiveness.IsExhaustive.ShouldBeTrue();
        exhaustiveness.MissingCases.ShouldBeEmpty();
    }

    [Fact]
    public void GetOperation_AssignmentStatement_ReturnsAssignmentOperation()
    {
        const string source = """
var value = 0
value = 1
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Last();

        var operation = model.GetOperation(assignment);

        operation.ShouldNotBeNull();
        operation!.Kind.ShouldBe(OperationKind.Assignment);
        operation.IsImplicit.ShouldBeFalse();
        operation.Syntax.ShouldBe(assignment);
    }

    [Fact]
    public void GetOperation_CompoundAssignmentStatement_ReturnsAssignmentOperation()
    {
        const string source = """
import System.*

delegate ChangedHandler(sender: object?, value: int) -> unit

class Source {
    event Changed: ChangedHandler?

    func Hook(handler: ChangedHandler) -> unit {
        Changed += handler
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var assignment = tree.GetRoot()
            .DescendantNodes()
            .OfType<AssignmentStatementSyntax>()
            .Single();

        var operation = model.GetOperation(assignment);

        operation.ShouldNotBeNull();
        operation!.Kind.ShouldBe(OperationKind.Assignment);
        operation.IsImplicit.ShouldBeFalse();
        operation.Syntax.ShouldBe(assignment);
        operation.ChildOperations.ShouldNotBeEmpty();
    }
}
