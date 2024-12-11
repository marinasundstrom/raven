
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public static class SyntaxFactory
{
    public static SyntaxToken IdentifierToken(string text) => new SyntaxToken(SyntaxKind.IdentifierToken, text);

    public static SyntaxToken NumericLiteral(int value) => new SyntaxToken(SyntaxKind.NumericLiteralToken, value.ToString());

    
    public static SyntaxTrivia Whitespace(string text) => new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, text);

    public static readonly SyntaxTrivia LineFeed = new SyntaxTrivia(SyntaxKind.EndOfLineToken, "\n");
    
    public static readonly SyntaxTrivia CarriageReturnLineFeed = new SyntaxTrivia(SyntaxKind.CarriageReturnLineFeedTrivia, "\r\n");

    public static readonly SyntaxTrivia Space = new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, " ");
    
    public static readonly SyntaxTrivia Tab = new SyntaxTrivia(SyntaxKind.TabTrivia, "\t");
    

    public static readonly SyntaxToken OpenParenToken = new SyntaxToken(SyntaxKind.OpenParenToken, "(");

    public static readonly SyntaxToken CloseParenToken = new SyntaxToken(SyntaxKind.CloseParenToken, ")");

    public static readonly SyntaxToken OpenBraceToken = new SyntaxToken(SyntaxKind.OpenBraceToken, "{");

    public static readonly SyntaxToken CloseBraceToken = new SyntaxToken(SyntaxKind.CloseBraceToken, "}");

    public static readonly SyntaxToken GreaterThanToken = new SyntaxToken(SyntaxKind.GreaterThanToken, ">");

    public static readonly SyntaxToken ImportKeyword = new SyntaxToken(SyntaxKind.LetKeyword, "import");

    public static readonly SyntaxToken NamespaceKeyword = new SyntaxToken(SyntaxKind.LetKeyword, "namespace");

    public static readonly SyntaxToken LetKeyword = new SyntaxToken(SyntaxKind.LetKeyword, "let");

    public static readonly SyntaxToken IfKeyword = new SyntaxToken(SyntaxKind.IfKeyword, "if");

    public static readonly SyntaxToken ElseKeyword = new SyntaxToken(SyntaxKind.ElseKeyword, "else");

    public static readonly SyntaxToken ReturnKeyword = new SyntaxToken(SyntaxKind.ReturnKeyword, "return");

    public static readonly SyntaxToken EqualsToken = new SyntaxToken(SyntaxKind.EqualsToken, "=");

    public static readonly SyntaxToken CommaToken = new SyntaxToken(SyntaxKind.CommaToken, ",");

    public static readonly SyntaxToken ColonToken = new SyntaxToken(SyntaxKind.ColonToken, ":");

    public static readonly SyntaxToken SemicolonToken = new SyntaxToken(SyntaxKind.SemicolonToken, ";");

    public static readonly SyntaxToken EndOfLine = new SyntaxToken(SyntaxKind.EndOfLineToken, "\n");

    public static readonly SyntaxToken CarriageReturn = new SyntaxToken(SyntaxKind.CarriageReturnToken, "\r");
    
    public static readonly SyntaxToken EndOfFile = new SyntaxToken(SyntaxKind.EndOfFileToken, string.Empty);
}