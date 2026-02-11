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
    public void ParenthesizedLambda_WithLeadingAttribute_ParsesAndAppliesToParameter()
    {
        var lambda = ParseLambdaInitializer("[FromBody](content: string) => content");
        var parameter = Assert.Single(lambda.ParameterList.Parameters);
        var attributeList = Assert.Single(parameter.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);
        var attributeName = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("FromBody", attributeName.Identifier.Text);
    }

    [Fact]
    public void ParenthesizedLambda_WithLeadingReturnAttribute_ParsesAndAppliesToReturnType()
    {
        var lambda = ParseLambdaInitializer("[return: ReturnAttr] (x: int) -> int => x");

        Assert.NotNull(lambda.ReturnType);
        var returnAttributeList = Assert.Single(lambda.ReturnType!.AttributeLists);
        Assert.NotNull(returnAttributeList.Target);
        Assert.Equal("return", returnAttributeList.Target!.Identifier.Text);

        var parameter = Assert.Single(lambda.ParameterList.Parameters);
        Assert.Empty(parameter.AttributeLists);
    }

    [Fact]
    public void ParenthesizedLambda_WithLeadingReturnAttributeList_ParsesAllReturnAttributes()
    {
        var lambda = ParseLambdaInitializer("[return: A, B(\"Test\")] (x: int) -> int => x");

        Assert.NotNull(lambda.ReturnType);
        var returnAttributeList = Assert.Single(lambda.ReturnType!.AttributeLists);
        Assert.NotNull(returnAttributeList.Target);
        Assert.Equal("return", returnAttributeList.Target!.Identifier.Text);
        Assert.Equal(2, returnAttributeList.Attributes.Count);
    }

    [Fact]
    public void ParenthesizedLambda_WithLeadingReturnAndParameterAttributes_SplitsByTarget()
    {
        var lambda = ParseLambdaInitializer("[return: A][FromBody] (x: int) -> int => x");

        Assert.NotNull(lambda.ReturnType);
        var returnAttributeList = Assert.Single(lambda.ReturnType!.AttributeLists);
        Assert.NotNull(returnAttributeList.Target);
        Assert.Equal("return", returnAttributeList.Target!.Identifier.Text);
        Assert.Single(returnAttributeList.Attributes);

        var parameter = Assert.Single(lambda.ParameterList.Parameters);
        var parameterAttributeList = Assert.Single(parameter.AttributeLists);
        Assert.Null(parameterAttributeList.Target);
        Assert.Single(parameterAttributeList.Attributes);
    }

    [Fact]
    public void ParenthesizedLambda_WithDefaultParameterValue_Parses()
    {
        var expression = ParseExpression("(name: string, age: int = 1) => age");

        var lambda = Assert.IsType<ParenthesizedLambdaExpressionSyntax>(expression);
        Assert.Equal(2, lambda.ParameterList.Parameters.Count);
        Assert.Null(lambda.ParameterList.Parameters[0].DefaultValue);
        Assert.NotNull(lambda.ParameterList.Parameters[1].DefaultValue);
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

    private static ParenthesizedLambdaExpressionSyntax ParseLambdaInitializer(string initializer)
    {
        var tree = SyntaxTree.ParseText($"val f = {initializer}");
        var root = tree.GetRoot();

        Assert.Single(root.Members);
        var globalStatement = Assert.IsType<GlobalStatementSyntax>(root.Members[0]);
        var declarationStatement = Assert.IsType<LocalDeclarationStatementSyntax>(globalStatement.Statement);
        var declarator = Assert.Single(declarationStatement.Declaration.Declarators);
        var lambda = Assert.IsType<ParenthesizedLambdaExpressionSyntax>(declarator.Initializer!.Value);
        return lambda;
    }
}
