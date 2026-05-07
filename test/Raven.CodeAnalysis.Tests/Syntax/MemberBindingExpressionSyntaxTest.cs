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

    [Fact]
    public void TargetTypedConstructorBinding_ParsesAsInvocation()
    {
        var tree = SyntaxTree.ParseText(".(2, -1)");

        Assert.Empty(tree.GetDiagnostics());

        var root = tree.GetRoot();
        var stmt = ((GlobalStatementSyntax)root.Members[0]).Statement;
        var expression = Assert.IsType<ExpressionStatementSyntax>(stmt).Expression;
        var invocation = Assert.IsType<InvocationExpressionSyntax>(expression);
        var memberBinding = Assert.IsType<MemberBindingExpressionSyntax>(invocation.Expression);

        Assert.True(memberBinding.Name.Identifier.IsMissing);
        Assert.Equal(2, invocation.ArgumentList.Arguments.Count);
    }
}
