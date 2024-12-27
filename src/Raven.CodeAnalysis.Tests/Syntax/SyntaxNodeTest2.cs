namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SyntaxNodeTest2(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void SyntaxNode_WithNoTrivia()
    {
        var block = Block(
            OpenBraceToken,
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword,
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
                    SemicolonToken)
            ),
            CloseBraceToken
        );

        testOutputHelper.WriteLine(block.ToFullString());
    }

    [Fact]
    public void SyntaxNode_WithTrivia()
    {
        var block = Block(
            OpenBraceToken
                .WithLeadingTrivia(LineFeed)
                .WithTrailingTrivia(LineFeed),
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword.WithLeadingTrivia(Whitespace("    ")),
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42).WithLeadingTrivia(Whitespace(" "))),
                    SemicolonToken.WithTrailingTrivia(LineFeed))
                    .WithTrailingTrivia(LineFeed)
            ),
            CloseBraceToken
                .WithTrailingTrivia(LineFeed)
        );

        testOutputHelper.WriteLine(block.ToFullString());
    }

    [Fact]
    public void ReplaceTokenWithTokenWithTrivia()
    {
        var block = Block(
            OpenBraceToken,
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword,
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
                    SemicolonToken)
            ),
            CloseBraceToken
        );

        var newBlock = block.ReplaceToken(block.OpenBraceToken, block.OpenBraceToken.WithTrailingTrivia(SyntaxFactory.CarriageReturnLineFeed));

        newBlock.ShouldNotBeSameAs(block);

        testOutputHelper.WriteLine(block.ToFullString());
        testOutputHelper.WriteLine(newBlock.ToFullString());
    }

    [Fact]
    public void ReplaceNodeInListWithNode()
    {
        var block = Block(
            OpenBraceToken,
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword,
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
                    SemicolonToken)
            ),
            CloseBraceToken
        );

        var returnStatement = block.Statements.OfType<ReturnStatementSyntax>().First();

        var newChild = ExpressionStatement(LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(20)));

        var newBlock = block.ReplaceNode(returnStatement, newChild);

        newBlock.ShouldNotBeSameAs(block);

        testOutputHelper.WriteLine(block.ToFullString());
        testOutputHelper.WriteLine(newBlock.ToFullString());
    }
}