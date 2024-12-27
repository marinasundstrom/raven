namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxNodeTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void ReplaceNode()
    {
        var returnStatement = ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space),
                        LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
                        SemicolonToken);

        var literalExpression = returnStatement
            .ChildNodes()
            .OfType<LiteralExpressionSyntax>().First();

        var newLiteralExpression = LiteralExpression(SyntaxKind.StringLiteralToken, IdentifierToken("test"));

        var newReturnStatement = returnStatement.ReplaceNode(literalExpression, newLiteralExpression);

        newReturnStatement.ShouldNotBeSameAs(returnStatement);

        testOutputHelper.WriteLine(newReturnStatement.ToFullString());
    }

    [Fact]
    public void ReplaceToken()
    {
        var returnStatement = ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space),
                        LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
                        SemicolonToken);

        var literalExpression = returnStatement
            .ChildNodes()
            .OfType<LiteralExpressionSyntax>().First();

        var newToken = IdentifierToken("test");

        var newReturnStatement = returnStatement.ReplaceToken(literalExpression.Token, newToken);

        newReturnStatement.ShouldNotBeSameAs(returnStatement);

        testOutputHelper.WriteLine(newReturnStatement.ToFullString());
    }


    [Fact]
    public void ReplaceToken2()
    {
        var returnStatement = ReturnStatement(ReturnKeyword,
                        LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
                        SemicolonToken);

        var newToken = returnStatement.ReturnKeyword.WithTrailingTrivia(Space);

        var newReturnStatement = returnStatement.ReplaceToken(returnStatement.ReturnKeyword, newToken);

        newReturnStatement.ShouldNotBeSameAs(returnStatement);

        newReturnStatement.Span.Length.ShouldBe(returnStatement.Span.Length + 1);

        testOutputHelper.WriteLine(newReturnStatement.ToFullString());
    }
}