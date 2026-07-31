using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SemanticModelFlowOwnershipTests : CompilationTestBase
{
    [Fact]
    public void FlowAnalysis_RejectsNodesFromAnotherSyntaxTree()
    {
        var source = "func Test() {\nlet first = 1\nlet second = first + 1\n}";
        var modelTree = SyntaxTree.ParseText(source);
        var foreignTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(modelTree);
        var model = compilation.GetSemanticModel(modelTree);
        var foreignStatements = foreignTree.GetRoot()
            .DescendantNodes()
            .OfType<LocalDeclarationStatementSyntax>()
            .ToArray();

        Assert.Throws<ArgumentException>(() => model.AnalyzeDataFlow(foreignStatements[0]));
        Assert.Throws<ArgumentException>(() => model.AnalyzeDataFlow(foreignStatements[0], foreignStatements[1]));
        Assert.Throws<ArgumentException>(() => model.AnalyzeControlFlow(foreignStatements[0]));
        Assert.Throws<ArgumentException>(() => model.AnalyzeControlFlow(foreignStatements[0], foreignStatements[1]));
    }

    [Fact]
    public void FlowAnalysis_RejectsDetachedNodes()
    {
        var tree = SyntaxTree.ParseText("func Test() { let value = 1 }");
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var detachedExpression = SyntaxFactory.LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            SyntaxFactory.Literal(1));
        var detachedStatement = SyntaxFactory.ExpressionStatement(detachedExpression);

        Assert.Throws<ArgumentException>(() => model.AnalyzeDataFlow(detachedExpression));
        Assert.Throws<ArgumentException>(() => model.AnalyzeDataFlow(detachedStatement));
        Assert.Throws<ArgumentException>(() => model.AnalyzeControlFlow(detachedStatement));
    }

    [Fact]
    public void FlowAnalysis_ReversedStatementRegionDoesNotSucceed()
    {
        var tree = SyntaxTree.ParseText("func Test() {\nlet first = 1\nlet second = first + 1\n}");
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var statements = tree.GetRoot()
            .DescendantNodes()
            .OfType<LocalDeclarationStatementSyntax>()
            .ToArray();

        Assert.False(model.AnalyzeDataFlow(statements[1], statements[0]).Succeeded);
        Assert.False(model.AnalyzeControlFlow(statements[1], statements[0]).Succeeded);
        Assert.Throws<ArgumentException>(() => new ControlFlowRegion(statements[1], statements[0]));
    }
}
