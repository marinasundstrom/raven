namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Tests;

public class GreenTreeTest2
{
    [Fact]
    public void Test1()
    {
        var node = SyntaxFactory.ReturnStatement(
            SyntaxFactory.ReturnKeyword,
            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.NumericLiteral(2)),
            SyntaxFactory.Token(SyntaxKind.None)
        );

        var n = node.CreateRed();
        var str = n.ToFullString();
    }

    [Fact]
    public void Test2()
    {
        var node = SyntaxFactory.ReturnStatement(
            SyntaxFactory.ReturnKeyword,
            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.NumericLiteral(2)),
            SyntaxFactory.SemicolonToken
        );

        var n = node.CreateRed();
        var str = n.ToFullString();
    }

}
