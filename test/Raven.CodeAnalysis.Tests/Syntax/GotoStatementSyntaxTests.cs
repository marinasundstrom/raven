using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class GotoStatementSyntaxTests
{
    [Fact]
    public void ParsesGotoStatement()
    {
        var tree = SyntaxTree.ParseText("goto start\n");
        var gotoStatement = tree.GetRoot().DescendantNodes().OfType<GotoStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.GotoStatement, gotoStatement.Kind);
        Assert.Equal("start", gotoStatement.Identifier.Text);
    }
}
