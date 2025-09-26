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
    }
}
