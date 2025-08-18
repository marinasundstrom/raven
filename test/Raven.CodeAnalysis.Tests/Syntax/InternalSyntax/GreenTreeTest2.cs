namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Tests;

public class GreenTreeTest2
{
    [Fact]
    public void Test1()
    {
        var node = new ReturnStatementSyntax(
            new SyntaxToken(SyntaxKind.ReturnKeyword, "return"),
            new LiteralExpressionSyntax(SyntaxKind.NumericLiteralExpression, new SyntaxToken(SyntaxKind.NumericLiteralToken, "2")),
            new SyntaxToken(SyntaxKind.SemicolonToken, ";")
        );
    }

    [Fact]
    public void Test2()
    {
        var node = new ReturnStatementSyntax(
            new SyntaxToken(SyntaxKind.ReturnKeyword, "return", null!, new SyntaxTriviaList([new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, " ")])),
            new LiteralExpressionSyntax(SyntaxKind.NumericLiteralExpression, new SyntaxToken(SyntaxKind.NumericLiteralToken, "2")),
            new SyntaxToken(SyntaxKind.SemicolonToken, ";")
        );
    }

}
