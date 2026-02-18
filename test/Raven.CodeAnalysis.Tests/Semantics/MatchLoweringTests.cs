using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class MatchLoweringTests
{
    [Fact]
    public void MatchExpression_LoweredView_ContainsNoBoundMatchNodes()
    {
        const string code = """
val result = 1 match {
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
}
