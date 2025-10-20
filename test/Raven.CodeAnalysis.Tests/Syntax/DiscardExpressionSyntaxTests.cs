using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class DiscardExpressionSyntaxTests
{
    [Fact]
    public void UnderscoreExpression_ParsesAsDiscardExpression()
    {
        var tree = SyntaxTree.ParseText("_");
        var root = tree.GetRoot();
        var statement = Assert.IsType<ExpressionStatementSyntax>(((GlobalStatementSyntax)root.Members[0]).Statement);
        Assert.IsType<DiscardExpressionSyntax>(statement.Expression);
    }
}
