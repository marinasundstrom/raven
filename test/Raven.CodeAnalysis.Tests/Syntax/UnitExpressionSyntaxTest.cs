using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class UnitExpressionSyntaxTest
{
    [Fact]
    public void UnitLiteral_Parses()
    {
        var tree = SyntaxTree.ParseText("()");
        var root = tree.GetRoot();
        var expr = ((GlobalStatementSyntax)root.Members[0]).Statement;
        var expression = Assert.IsType<ExpressionStatementSyntax>(expr).Expression;
        Assert.IsType<UnitExpressionSyntax>(expression);
    }
}
