using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class YieldStatementSyntaxTests
{
    [Fact]
    public void YieldReturnForm_ReportsMigrationDiagnostic()
    {
        var tree = SyntaxTree.ParseText("yield return value\n");
        var yieldStatement = tree.GetRoot().DescendantNodes().OfType<YieldStatementSyntax>().Single();

        Assert.Contains(tree.GetDiagnostics(), diagnostic => diagnostic.Descriptor == CompilerDiagnostics.YieldReturnFormRemoved);
        var recovery = Assert.IsType<ReturnExpressionSyntax>(yieldStatement.Expression);
        Assert.Equal("value", ((IdentifierNameSyntax)recovery.Expression!).Identifier.Text);
    }

    [Fact]
    public void ParsesYieldStatement()
    {
        var tree = SyntaxTree.ParseText("yield value\n");
        var yieldStatement = tree.GetRoot().DescendantNodes().OfType<YieldStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.YieldStatement, yieldStatement.Kind);
        Assert.Equal("value", ((IdentifierNameSyntax)yieldStatement.Expression).Identifier.Text);
    }

    [Fact]
    public void YieldBreakForm_ReportsMigrationDiagnostic()
    {
        var tree = SyntaxTree.ParseText("yield break\n");
        var yieldStatement = tree.GetRoot().DescendantNodes().OfType<YieldStatementSyntax>().Single();

        Assert.Contains(tree.GetDiagnostics(), diagnostic => diagnostic.Descriptor == CompilerDiagnostics.YieldBreakFormRemoved);
        Assert.IsType<BreakExpressionSyntax>(yieldStatement.Expression);
    }
}
