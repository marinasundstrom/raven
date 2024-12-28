﻿namespace Raven.CodeAnalysis.Syntax.Tests;

using System.Security.Principal;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public partial class SyntaxNodeTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void ReplaceNode_DirectChild()
    {
        var ifStatement = IfStatement(
            IfKeyword,
            OpenParenToken,
            LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
            CloseParenToken,
            Block([]));

        var condition = ifStatement.Condition;

        var newCondition = LiteralExpression(SyntaxKind.StringLiteralExpression, IdentifierToken("test"));

        var newIfStatement = (IfStatementSyntax)ifStatement.ReplaceNode(condition, newCondition);

        newIfStatement.ShouldNotBeSameAs(ifStatement);
        newIfStatement.Condition.ShouldBeOfType<LiteralExpressionSyntax>();
        testOutputHelper.WriteLine(newIfStatement.ToFullString());
    }

    [Fact]
    public void ReplaceNode_WithinListInNode()
    {
        var block = Block(
            OpenBraceToken,
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)), SemicolonToken),
                ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(24)), SemicolonToken)
            ),
            CloseBraceToken);

        var oldReturnStatement = block.Statements[1];
        var newReturnStatement = ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(100)), SemicolonToken);

        var newBlock = (BlockSyntax)block.ReplaceNode(oldReturnStatement, newReturnStatement);

        newBlock.ShouldNotBeSameAs(block);
        newBlock.Statements.Count.ShouldBe(2);
        newBlock.Statements[1].ToFullString().ShouldContain("return 100;");
        testOutputHelper.WriteLine(newBlock.ToFullString());
    }

    [Fact]
    public void ReplaceToken_DirectChild()
    {
        var ifStatement = IfStatement(
            IfKeyword.WithTrailingTrivia(Space),
            OpenParenToken,
            LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
            CloseParenToken,
            Block([]));

        var newIfKeyword = IdentifierToken("test").WithTrailingTrivia(Space);

        var newIfStatement = (IfStatementSyntax)ifStatement.ReplaceToken(ifStatement.IfKeyword, newIfKeyword);

        newIfStatement.ShouldNotBeSameAs(ifStatement);
        newIfStatement.IfKeyword.Text.ShouldBe("test");
        testOutputHelper.WriteLine(newIfStatement.ToFullString());
    }

    [Fact]
    public void ReplaceToken_WithinInnerNode()
    {
        var ifStatement = IfStatement(
            IfKeyword,
            OpenParenToken,
            LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)),
            CloseParenToken,
            Block([]));

        var oldToken = ifStatement.Condition.GetFirstToken();
        var newToken = NumericLiteral(100);

        var newIfStatement = (IfStatementSyntax)ifStatement.ReplaceToken(oldToken, newToken);

        newIfStatement.ShouldNotBeSameAs(ifStatement);
        newIfStatement.Condition.ToFullString().ShouldContain("100");
        testOutputHelper.WriteLine(newIfStatement.ToFullString());
    }

    [Fact]
    public void ReplaceNode_EntireList()
    {
        var block = Block(
            OpenBraceToken.WithTrailingTrivia(Space),
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)), SemicolonToken),
                ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(24)), SemicolonToken)
            ),
            CloseBraceToken);

        var newStatements = List<StatementSyntax>(
            ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(100)), SemicolonToken),
            ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(200)), SemicolonToken)
        );

        var newBlock = block.WithStatements(newStatements);

        newBlock.ShouldNotBeSameAs(block);
        newBlock.Statements.Count.ShouldBe(2);
        newBlock.Statements[0].ToFullString().ShouldContain("return 100;");
        newBlock.Statements[1].ToFullString().ShouldContain("return 200;");
        testOutputHelper.WriteLine(newBlock.ToFullString());
    }

    [Fact]
    public void ReplaceToken_WithinListInNode()
    {
        var block = Block(
            OpenBraceToken.WithTrailingTrivia(Space),
            List<StatementSyntax>(
                ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(42)), SemicolonToken),
                ReturnStatement(ReturnKeyword.WithTrailingTrivia(Space), LiteralExpression(SyntaxKind.NumericLiteralExpression, NumericLiteral(24)), SemicolonToken)
            ),
            CloseBraceToken);

        // Get the second return statement
        var oldReturnStatement = (ReturnStatementSyntax)block.Statements[1];

        // Get the token for the numeric literal in the expression
        var oldToken = oldReturnStatement.Expression!.GetFirstToken();

        // Create the new token
        var newToken = NumericLiteral(100);

        // Replace the token in the block
        var newBlock = (BlockSyntax)block.ReplaceToken(oldToken, newToken);

        // Assertions
        newBlock.ShouldNotBeSameAs(block);

        // Validate that the second return statement has the updated literal
        var updatedReturnStatement = (ReturnStatementSyntax)newBlock.Statements[1];
        updatedReturnStatement.Expression!.ToFullString().ShouldBe("100");

        // Validate the full text of the block
        newBlock.Statements[1].ToFullString().ShouldContain("return 100;");
        testOutputHelper.WriteLine(newBlock.ToFullString());
    }
}