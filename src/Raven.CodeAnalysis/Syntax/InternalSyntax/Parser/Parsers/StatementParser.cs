namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class StatementParser : SyntaxParser
{
    public StatementParser(ParseContext parent) : base(parent)
    {

    }

    public StatementSyntax ParseStatement()
    {
        var token = PeekToken();

        // Placeholder literal expression as a stub
        var expr = SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, token);
        var semi = ReadToken(); // simulate consumption

        return SyntaxFactory.ExpressionStatementWithSemicolon(expr, semi, []);
    }
}