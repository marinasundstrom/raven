using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Lowering.Tests;

public class MatchLoweringTests
{
    [Fact]
    public void MatchExpression_LoweredView_ContainsNoBoundMatchNodes()
    {
        const string code = """
val result = match 1 {
    1 => 10
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_lowered_shape",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var lowered = model.GetBoundNode(match, BoundTreeView.Lowered);

        var finder = new MatchNodeFinder();
        VisitLoweredNode(finder, lowered);

        Assert.Equal(0, finder.MatchExpressionCount);
        Assert.Equal(0, finder.MatchStatementCount);
    }

    [Fact]
    public void MatchStatement_LoweredView_ContainsNoBoundMatchNodes()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    match flag {
        true => 1
        false => 0
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_lowered_shape",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var lowered = model.GetBoundNode(match, BoundTreeView.Lowered);

        var finder = new MatchNodeFinder();
        VisitLoweredNode(finder, lowered);

        Assert.Equal(0, finder.MatchExpressionCount);
        Assert.Equal(0, finder.MatchStatementCount);
    }

    [Fact]
    public void MatchExpression_LoweredView_UsesScrutineeAndResultLocalsWithBranchAssignments()
    {
        const string code = """
func next() -> int {
    return 1
}

val result = match next() {
    1 => 10
    _ => 0
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_expression_lowered_control_flow",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();
        var lowered = Assert.IsType<BoundBlockExpression>(model.GetBoundNode(match, BoundTreeView.Lowered));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(7, statements.Length);

        var scrutineeDeclaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);
        var scrutineeDeclarator = Assert.Single(scrutineeDeclaration.Declarators);
        Assert.IsType<BoundInvocationExpression>(scrutineeDeclarator.Initializer);

        var resultDeclaration = Assert.IsType<BoundLocalDeclarationStatement>(statements[1]);
        var resultLocal = Assert.Single(resultDeclaration.Declarators).Local;

        var firstArm = Assert.IsType<BoundIfStatement>(statements[2]);
        Assert.IsType<BoundIsPatternExpression>(firstArm.Condition);
        AssertAssignsMatchResult(firstArm.ThenNode, resultLocal, expectedValue: 10);

        var fallbackArm = Assert.IsType<BoundIfStatement>(statements[3]);
        Assert.IsType<BoundIsPatternExpression>(fallbackArm.Condition);
        AssertAssignsMatchResult(fallbackArm.ThenNode, resultLocal, expectedValue: 0);

        Assert.IsType<BoundThrowStatement>(statements[4]);
        Assert.IsType<BoundLabeledStatement>(statements[5]);
        var finalExpression = Assert.IsType<BoundExpressionStatement>(statements[6]);
        var finalAccess = Assert.IsType<BoundLocalAccess>(finalExpression.Expression);
        Assert.Same(resultLocal, finalAccess.Local);
    }

    [Fact]
    public void MatchStatement_LoweredView_EmitsFinalDiscardArmDirectly()
    {
        const string code = """
func evaluate(flag: bool) -> int {
    var result = 0
    match flag {
        true => result = 1
        _ => result = 2
    }
    return result
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "match_statement_lowered_final_discard",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        compilation.EnsureSetup();
        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchStatementSyntax>().Single();
        var lowered = Assert.IsType<BoundBlockStatement>(model.GetBoundNode(match, BoundTreeView.Lowered));
        var statements = lowered.Statements.ToArray();

        Assert.Equal(5, statements.Length);

        Assert.IsType<BoundLocalDeclarationStatement>(statements[0]);

        var firstArm = Assert.IsType<BoundIfStatement>(statements[1]);
        Assert.IsType<BoundIsPatternExpression>(firstArm.Condition);

        var fallbackBlock = Assert.IsType<BoundBlockStatement>(statements[2]);
        var fallbackStatements = fallbackBlock.Statements.ToArray();
        Assert.Equal(2, fallbackStatements.Length);
        Assert.IsType<BoundAssignmentStatement>(fallbackStatements[0]);
        Assert.IsType<BoundGotoStatement>(fallbackStatements[1]);

        Assert.IsType<BoundThrowStatement>(statements[3]);
        Assert.IsType<BoundLabeledStatement>(statements[4]);
    }

    private sealed class MatchNodeFinder : BoundTreeWalker
    {
        public int MatchExpressionCount { get; private set; }

        public int MatchStatementCount { get; private set; }

        public override void VisitMatchExpression(BoundMatchExpression node)
        {
            MatchExpressionCount++;
            base.VisitMatchExpression(node);
        }

        public override void VisitMatchStatement(BoundMatchStatement node)
        {
            MatchStatementCount++;
            base.VisitMatchStatement(node);
        }
    }

    private static void VisitLoweredNode(MatchNodeFinder finder, BoundNode lowered)
    {
        switch (lowered)
        {
            case BoundStatement statement:
                finder.VisitStatement(statement);
                break;
            case BoundExpression expression:
                finder.VisitExpression(expression);
                break;
            default:
                finder.Visit(lowered);
                break;
        }
    }

    private static void AssertAssignsMatchResult(BoundStatement statement, ILocalSymbol resultLocal, int expectedValue)
    {
        var block = Assert.IsType<BoundBlockStatement>(statement);
        var statements = block.Statements.ToArray();
        Assert.Equal(2, statements.Length);

        var assignmentStatement = Assert.IsType<BoundAssignmentStatement>(statements[0]);
        var assignment = Assert.IsType<BoundLocalAssignmentExpression>(assignmentStatement.Expression);
        Assert.Same(resultLocal, assignment.Local);

        var valueExpression = assignment.Right is BoundRequiredResultExpression requiredResult
            ? requiredResult.Operand
            : assignment.Right;
        var value = Assert.IsType<BoundLiteralExpression>(valueExpression);
        Assert.Equal(expectedValue, value.Value);

        Assert.IsType<BoundGotoStatement>(statements[1]);
    }
}
