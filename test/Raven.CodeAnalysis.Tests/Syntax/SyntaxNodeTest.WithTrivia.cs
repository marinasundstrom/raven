using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public partial class SyntaxNodeTest
{
    [Fact]
    public void SyntaxNode_WithNoTrivia()
    {
        var block = Block(
            OpenBraceToken,
            List<StatementSyntax>(
                new ReturnStatementSyntax(ReturnKeyword,
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(42)),
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
                new ReturnStatementSyntax(ReturnKeyword.WithLeadingTrivia(Whitespace("    ")),
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(42).WithLeadingTrivia(Whitespace(" "))),
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
                new ReturnStatementSyntax(ReturnKeyword,
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(42)),
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
                new ReturnStatementSyntax(ReturnKeyword,
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(42)),
                    SemicolonToken)
            ),
            CloseBraceToken
        );

        var returnStatement = block.Statements.OfType<ReturnStatementSyntax>().First();

        var newChild = new ExpressionStatementSyntax(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(20)), Token(SyntaxKind.None));

        var newBlock = block.ReplaceNode(returnStatement, newChild);

        newBlock.ShouldNotBeSameAs(block);

        testOutputHelper.WriteLine(block.ToFullString());
        testOutputHelper.WriteLine(newBlock.ToFullString());
    }

    [Fact]
    public void ReplaceTokens_InsideStructuredTrivia()
    {
        var root = IdentifierName(TrueKeyword)
            .WithTrailingTrivia(
                Trivia(
                    SkippedTokensTrivia(
                        TokenList(Identifier("Original")))));
        var structuredTrivia = Assert.Single(root.DescendantTrivia(), trivia => trivia.HasStructure);
        var structure = Assert.IsAssignableFrom<StructuredTriviaSyntax>(structuredTrivia.GetStructure());
        var originalToken = Assert.Single(structure.DescendantTokens());
        var replacementToken = SyntaxFactory.Identifier("Updated");

        var updatedRoot = root.ReplaceTokens(
            [originalToken],
            (_, _) => replacementToken);

        Assert.Contains("Updated", updatedRoot.ToFullString(), StringComparison.Ordinal);
        Assert.DoesNotContain("Original", updatedRoot.ToFullString(), StringComparison.Ordinal);
    }
}
