using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class AwaitExpressionTests
{
    [Fact]
    public void AwaitExpression_ParsesAsUnaryExpression()
    {
        var lexer = new Lexer(new StringReader("await foo"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = Assert.IsAssignableFrom<ExpressionSyntax>(parser.ParseExpression().CreateRed());
        var awaitExpression = Assert.IsType<PrefixOperatorExpressionSyntax>(expression);

        Assert.Equal(SyntaxKind.AwaitExpression, awaitExpression.Kind);
        Assert.Equal(SyntaxKind.AwaitKeyword, awaitExpression.OperatorToken.Kind);

        var operand = Assert.IsType<IdentifierNameSyntax>(awaitExpression.Expression);
        Assert.Equal("foo", operand.Identifier.ValueText);
    }
    [Theory]
    [InlineData("await Foo()?", false)]
    [InlineData("try Foo()?", true)]
    public void Propagation_AppliesAfterPrefix(string source, bool isTry)
    {
        var propagation = Assert.IsType<PropagateExpressionSyntax>(Parse(source));
        var operand = PrefixOperand(propagation.Expression, isTry);
        Assert.IsType<InvocationExpressionSyntax>(operand);
    }

    [Theory]
    [InlineData("await (Foo()?)", false)]
    [InlineData("try (Foo()?)", true)]
    public void Parentheses_KeepPropagationInsidePrefix(string source, bool isTry)
    {
        var operand = Assert.IsType<ParenthesizedExpressionSyntax>(PrefixOperand(Parse(source), isTry));
        Assert.IsType<PropagateExpressionSyntax>(operand.Expression);
    }

    [Theory]
    [InlineData("await Foo(Bar()?)?", false)]
    [InlineData("try Foo(Bar()?)?", true)]
    public void ArgumentPropagation_StaysInsideArgument(string source, bool isTry)
    {
        var propagation = Assert.IsType<PropagateExpressionSyntax>(Parse(source));
        var invocation = Assert.IsType<InvocationExpressionSyntax>(PrefixOperand(propagation.Expression, isTry));
        Assert.IsType<PropagateExpressionSyntax>(invocation.ArgumentList.Arguments[0].Expression);
    }

    [Theory]
    [InlineData("await Foo()?.Bar()", false)]
    [InlineData("try Foo()?.Bar()", true)]
    public void ConditionalAccess_StaysInsidePrefix(string source, bool isTry)
    {
        Assert.IsType<ConditionalAccessExpressionSyntax>(PrefixOperand(Parse(source), isTry));
    }

    [Theory]
    [InlineData("await Foo()? match { _ => 42 }")]
    [InlineData("try Foo()? match { _ => 42 }")]
    public void Match_FollowsPropagation(string source)
    {
        var match = Assert.IsType<PostfixMatchExpressionSyntax>(Parse(source));
        Assert.IsType<PropagateExpressionSyntax>(match.Expression);
    }

    [Fact]
    public void TryAwait_PropagatesAfterBothPrefixes()
    {
        var propagation = Assert.IsType<PropagateExpressionSyntax>(Parse("try await Foo()?"));
        var capture = Assert.IsType<TryExpressionSyntax>(propagation.Expression);
        Assert.IsType<InvocationExpressionSyntax>(PrefixOperand(capture.Expression, false));
    }

    [Fact]
    public void AwaitPropagation_PrecedesAddition()
    {
        var addition = Assert.IsType<InfixOperatorExpressionSyntax>(Parse("await Foo()? + 1"));
        Assert.IsType<PropagateExpressionSyntax>(addition.Left);
    }

    private static ExpressionSyntax PrefixOperand(ExpressionSyntax expression, bool isTry)
    {
        if (isTry)
            return Assert.IsType<TryExpressionSyntax>(expression).Expression;

        var awaitExpression = Assert.IsType<PrefixOperatorExpressionSyntax>(expression);
        Assert.Equal(SyntaxKind.AwaitExpression, awaitExpression.Kind);
        return awaitExpression.Expression;
    }

    private static ExpressionSyntax Parse(string source)
    {
        var lexer = new Lexer(new StringReader(source));
        var parser = new ExpressionSyntaxParser(new BaseParseContext(lexer));
        var expression = (ExpressionSyntax)parser.ParseExpression().CreateRed();
        Assert.Equal(source, expression.ToFullString());
        Assert.Empty(expression.GetDiagnostics());
        return expression;
    }

}
