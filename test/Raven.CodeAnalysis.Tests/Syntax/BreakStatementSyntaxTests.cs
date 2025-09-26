using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class BreakStatementSyntaxTests
{
    [Fact]
    public void ParsesBreakStatement()
    {
        var tree = SyntaxTree.ParseText("break\n");
        var breakStatement = tree.GetRoot().DescendantNodes().OfType<BreakStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.BreakStatement, breakStatement.Kind);
    }
}
