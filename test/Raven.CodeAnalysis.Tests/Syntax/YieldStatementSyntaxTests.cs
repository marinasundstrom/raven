using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class YieldStatementSyntaxTests
{
    [Fact]
    public void ParsesYieldReturnStatement()
    {
        var tree = SyntaxTree.ParseText("yield return value\n");
        var yieldReturn = tree.GetRoot().DescendantNodes().OfType<YieldReturnStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.YieldReturnStatement, yieldReturn.Kind);
        Assert.Equal("value", ((IdentifierNameSyntax)yieldReturn.Expression).Identifier.Text);
    }

    [Fact]
    public void ParsesYieldBreakStatement()
    {
        var tree = SyntaxTree.ParseText("yield break\n");
        var yieldBreak = tree.GetRoot().DescendantNodes().OfType<YieldBreakStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.YieldBreakStatement, yieldBreak.Kind);
    }
}
