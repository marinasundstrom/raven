using Xunit;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Tests;

using static SyntaxFactory;

using SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind;

public class AstTest
{
    [Fact]
    public void Test1()
    {
        /*
        // Create tokens and nodes for statements inside the block
        var statement1 = new SyntaxNode(SyntaxKind.IfStatement,
        [
            IfKeyword,
                OpenParenToken,
                new SyntaxNode(SyntaxKind.GreaterThanExpression,
                [
                    IdentifierToken("a"),
                    GreaterThanToken,
                    IdentifierToken("b")
                ]),
                CloseParenToken,
                new SyntaxNode(SyntaxKind.Block,
                [
                    // Inner block (empty for simplicity)
                    OpenBraceToken,
                    CloseBraceToken
                ])
        // ElseClause is optional and omitted here
        ]);

        // Create a SyntaxList containing the statements
        var statementsItems = new SyntaxNode[]
        {
             statement1
            // Add more statements as needed
        };
        var statementsList = new SyntaxList(statementsItems);

        // Create the internal Block node
        var block = new BlockSyntax(
            OpenBraceToken,
            statementsList,
            CloseBraceToken
        );
        */
    }
}