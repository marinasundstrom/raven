using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class LambdaExpressionSyntaxTests
{
    [Fact]
    public void SimpleLambda_WithIdentifierParameter_Parses()
    {
        var expression = ParseExpression("value => value");

        var lambda = Assert.IsType<SimpleLambdaExpressionSyntax>(expression);
        Assert.Equal("value", lambda.Parameter.Identifier.Text);
    }

    [Fact]
    public void ParenthesizedLambda_WithMultipleParameters_Parses()
    {
        var expression = ParseExpression("(left, right) => left + right");

        var lambda = Assert.IsType<ParenthesizedLambdaExpressionSyntax>(expression);
        Assert.Collection(
            lambda.ParameterList.Parameters,
            parameter => Assert.Equal("left", parameter.Identifier.Text),
            parameter => Assert.Equal("right", parameter.Identifier.Text));
    }

    [Fact]
    public void SimpleLambda_WithAsyncModifier_Parses()
    {
        var expression = ParseExpression("async value => value");

        var lambda = Assert.IsType<SimpleLambdaExpressionSyntax>(expression);
        Assert.True(lambda.AsyncKeyword.HasValue);
        Assert.Equal(SyntaxKind.AsyncKeyword, lambda.AsyncKeyword!.Value.Kind);
    }

    [Fact]
    public void ParenthesizedLambda_WithAsyncModifier_Parses()
    {
        var expression = ParseExpression("async () => 42");

        var lambda = Assert.IsType<ParenthesizedLambdaExpressionSyntax>(expression);
        Assert.True(lambda.AsyncKeyword.HasValue);
        Assert.Equal(SyntaxKind.AsyncKeyword, lambda.AsyncKeyword!.Value.Kind);
    }

    [Fact]
    public void ParenthesizedLambda_WithoutParameters_Parses()
    {
        var expression = ParseExpression("() => 42");

        Assert.IsType<ParenthesizedLambdaExpressionSyntax>(expression);
    }

    [Fact]
    public void ParenthesizedExpression_WithSingleIdentifier_DoesNotParseAsLambda()
    {
        var expression = ParseExpression("(value)");

        Assert.IsType<ParenthesizedExpressionSyntax>(expression);
    }

    [Fact]
    public void TupleExpression_WithMultipleElements_DoesNotParseAsLambda()
    {
        var expression = ParseExpression("(first, second)");

        Assert.IsType<TupleExpressionSyntax>(expression);
    }

    [Fact]
    public void UnitExpression_WithoutArrow_DoesNotParseAsLambda()
    {
        var expression = ParseExpression("()");

        Assert.IsType<UnitExpressionSyntax>(expression);
    }

    private static ExpressionSyntax ParseExpression(string code)
    {
        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        Assert.Single(root.Members);
        var globalStatement = Assert.IsType<GlobalStatementSyntax>(root.Members[0]);
        var statement = Assert.IsType<ExpressionStatementSyntax>(globalStatement.Statement);

        return statement.Expression;
    }
}
