using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class LabeledStatementSyntaxTests
{
    [Fact]
    public void ParsesLabeledStatement()
    {
        var tree = SyntaxTree.ParseText("start:\n    goto start\n");
        var labeledStatement = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.LabeledStatement, labeledStatement.Kind);
        Assert.Equal("start", labeledStatement.Identifier.Text);
        Assert.IsType<GotoStatementSyntax>(labeledStatement.Statement);
    }
}
