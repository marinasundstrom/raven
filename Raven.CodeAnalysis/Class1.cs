using Raven.CodeAnalysis.Syntax.InternalSyntax;

using SyntaxKind = Raven.CodeAnalysis.Syntax.SyntaxKind;

namespace SyntaxExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // Create tokens for the block
            var openBrace = new SyntaxToken(SyntaxKind.OpenBraceToken, "{");
            var closeBrace = new SyntaxToken(SyntaxKind.CloseBraceToken, "}");

            // Create tokens and nodes for statements inside the block
            var statement1 = new SyntaxNode(SyntaxKind.IfStatement,
            [
                new SyntaxToken(SyntaxKind.IfKeyword, "if"),
                new SyntaxToken(SyntaxKind.OpenParenToken, "("),
                new SyntaxNode(SyntaxKind.ConditionExpression,
                [
                    new SyntaxToken(SyntaxKind.IdentifierToken, "a"),
                    new SyntaxToken(SyntaxKind.GreaterThanToken, ">"),
                    new SyntaxToken(SyntaxKind.IdentifierToken, "b")
                ], "a > b".Length),
                new SyntaxToken(SyntaxKind.CloseParenToken, ")"),
                new SyntaxNode(SyntaxKind.Block,
                [
                    // Inner block (empty for simplicity)
                    new SyntaxToken(SyntaxKind.OpenBraceToken, "{"),
                    new SyntaxToken(SyntaxKind.CloseBraceToken, "}")
                ], "{}".Length)
                // ElseClause is optional and omitted here
            ], "if (a > b) {}".Length);

            // Create a SyntaxList containing the statements
            var statementsItems = new SyntaxListItem[]
            {
                new SyntaxListItem(statement1)
                // Add more statements as needed
            };
            var statementsList = new SyntaxList(statementsItems);

            // Create the internal Block node
            var block = new BlockSyntax(
                openBrace,
                statementsList,
                closeBrace,
                startPosition: 0
            );

            /*

            // Convert the internal Block node to the external BlockSyntax node
            Raven.CodeAnalysis.Syntax.BlockSyntax blockSyntax = SyntaxFactory.CreateWrapper(block) as BlockSyntax;

            // Use the external BlockSyntax node
            if (blockSyntax != null)
            {
                Console.WriteLine("Block Syntax:");
                Console.WriteLine($"Start Position: {blockSyntax.StartPosition}");
                Console.WriteLine($"End Position: {blockSyntax.EndPosition}");
                Console.WriteLine($"Full Width: {blockSyntax.FullWidth}");

                Console.WriteLine($"Open Brace: {blockSyntax.OpenBraceToken.Text}");

                Console.WriteLine("Statements:");
                foreach (var statement in blockSyntax.Statements)
                {
                    if (statement is Raven.CodeAnalysis.Syntax.IfStatementSyntax ifStmt)
                    {
                        Console.WriteLine("If Statement:");
                        //Console.WriteLine($"  Condition: {((SyntaxToken)ifStmt.Condition.ChildNodes[0]).Text} {((SyntaxToken)ifStmt.Condition.ChildNodes[1]).Text} {((SyntaxToken)ifStmt.Condition.ChildNodes[2]).Text}");
                        Console.WriteLine($"  Statement: {ifStmt.Statement.Kind}");
                    }
                    // Handle other statement types as needed
                }

                Console.WriteLine($"Close Brace: {blockSyntax.CloseBraceToken.Text}");
            }

            */
        }
    }
}
