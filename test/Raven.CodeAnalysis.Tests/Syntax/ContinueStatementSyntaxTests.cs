using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class ContinueStatementSyntaxTests
{
    [Fact]
    public void ParsesContinueStatement()
    {
        var tree = SyntaxTree.ParseText("continue\n");
        var continueStatement = tree.GetRoot().DescendantNodes().OfType<ContinueStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.ContinueStatement, continueStatement.Kind);
        Assert.Equal(SyntaxKind.None, continueStatement.Identifier.Kind);
    }

    [Fact]
    public void ParsesLabeledContinueStatement()
    {
        var tree = SyntaxTree.ParseText("continue outer\n");
        var continueStatement = tree.GetRoot().DescendantNodes().OfType<ContinueStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.ContinueStatement, continueStatement.Kind);
        Assert.Equal("outer", continueStatement.Identifier.ValueText);
    }

    [Fact]
    public void NewlineAfterContinueDoesNotParseLabel()
    {
        var tree = SyntaxTree.ParseText("""
continue
outer:
()
""");
        var continueStatement = tree.GetRoot().DescendantNodes().OfType<ContinueStatementSyntax>().Single();

        Assert.Equal(SyntaxKind.None, continueStatement.Identifier.Kind);
    }
}
