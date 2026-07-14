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
        Assert.Equal(SyntaxKind.None, breakStatement.Identifier.Kind);
    }

    [Fact]
    public void ParsesLabeledBreakStatement()
    {
        var tree = SyntaxTree.ParseText("break outer\n");
        var breakStatement = tree.GetRoot().DescendantNodes().OfType<BreakStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.BreakStatement, breakStatement.Kind);
        Assert.Equal("outer", breakStatement.Identifier.ValueText);
    }

    [Fact]
    public void NewlineAfterBreakDoesNotParseLabel()
    {
        var tree = SyntaxTree.ParseText("""
break
outer:
()
""");
        var breakStatement = tree.GetRoot().DescendantNodes().OfType<BreakStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.None, breakStatement.Identifier.Kind);
    }
}
