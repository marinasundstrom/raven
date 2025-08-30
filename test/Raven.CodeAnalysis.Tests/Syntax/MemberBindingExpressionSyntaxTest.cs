using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class MemberBindingExpressionSyntaxTest
{
    [Fact]
    public void MemberBinding_Parses()
    {
        var tree = SyntaxTree.ParseText(".Foo");
        var root = tree.GetRoot();
        var stmt = ((GlobalStatementSyntax)root.Members[0]).Statement;
        var expression = Assert.IsType<ExpressionStatementSyntax>(stmt).Expression;
        Assert.IsType<MemberBindingExpressionSyntax>(expression);
    }
}
