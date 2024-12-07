using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;
public class AstTest
{
    [Fact]
    public void Test1()
    {
        /*
        // Create tokens and nodes for statements inside the block
        var statement1 = new InternalSyntax.IfStatementSyntax(
            IfKeyword,
            OpenParenToken,
            new InternalSyntax.SyntaxNode(SyntaxKind.ConditionExpression,
            [
                Identifier("a"),
                GreaterThanToken,
                Identifier("b")
            ], "a > b".Length),
            CloseParenToken,
            new InternalSyntax.BlockSyntax(OpenBraceToken, InternalSyntax.SyntaxList.Empty, CloseBraceToken, "{}".Length),
            "if (a > b) {}".Length);

        var foo = new IfStatementSyntax(statement1);

        var s = foo.Statement;

        var c = foo.ChildNodesAndTokens();
        foreach (var e in c)
        {

        }

        var f = foo.TrailingTrivia;
        */
    }
}
