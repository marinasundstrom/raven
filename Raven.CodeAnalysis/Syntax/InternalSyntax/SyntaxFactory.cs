
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public static class SyntaxFactory
{
    public static SyntaxToken MissingToken(SyntaxKind kind) => SyntaxToken.Missing(kind);


    public static SyntaxToken IdentifierToken(string text) => new SyntaxToken(SyntaxKind.IdentifierToken, text);
    public static SyntaxToken NumericLiteral(int value) => new SyntaxToken(SyntaxKind.NumericLiteralToken, value.ToString());

    public static readonly SyntaxToken VoidKeyword = new SyntaxToken(SyntaxKind.VoidKeyword, "void");
    public static readonly SyntaxToken IntKeyword = new SyntaxToken(SyntaxKind.IntKeyword, "int");
    public static readonly SyntaxToken StringKeyword = new SyntaxToken(SyntaxKind.StringKeyword, "string");
    public static readonly SyntaxToken BoolKeyword = new SyntaxToken(SyntaxKind.BoolKeyword, "bool");
    public static readonly SyntaxToken CharKeyword = new SyntaxToken(SyntaxKind.CharKeyword, "char");
    public static readonly SyntaxToken ImportKeyword = new SyntaxToken(SyntaxKind.ImportKeyword, "import");
    public static readonly SyntaxToken NamespaceKeyword = new SyntaxToken(SyntaxKind.NamespaceKeyword, "namespace");
    public static readonly SyntaxToken LetKeyword = new SyntaxToken(SyntaxKind.LetKeyword, "let");
    public static readonly SyntaxToken IfKeyword = new SyntaxToken(SyntaxKind.IfKeyword, "if");
    public static readonly SyntaxToken ElseKeyword = new SyntaxToken(SyntaxKind.ElseKeyword, "else");
    public static readonly SyntaxToken ReturnKeyword = new SyntaxToken(SyntaxKind.ReturnKeyword, "return");
    public static readonly SyntaxToken TrueKeyword = new SyntaxToken(SyntaxKind.TrueKeyword, "true");
    public static readonly SyntaxToken FalseKeyword = new SyntaxToken(SyntaxKind.FalseKeyword, "false");
    public static readonly SyntaxToken NotKeyword = new SyntaxToken(SyntaxKind.NotKeyword, "not");

    public static readonly SyntaxToken OrToken = new SyntaxToken(SyntaxKind.OrToken, "|");
    public static readonly SyntaxToken AndToken = new SyntaxToken(SyntaxKind.AndToken, "&");

    public static readonly SyntaxToken OpenParenToken = new SyntaxToken(SyntaxKind.OpenParenToken, "(");
    public static readonly SyntaxToken CloseParenToken = new SyntaxToken(SyntaxKind.CloseParenToken, ")");
    public static readonly SyntaxToken OpenBraceToken = new SyntaxToken(SyntaxKind.OpenBraceToken, "{");
    public static readonly SyntaxToken CloseBraceToken = new SyntaxToken(SyntaxKind.CloseBraceToken, "}");

    public static readonly SyntaxToken GreaterThanToken = new SyntaxToken(SyntaxKind.GreaterThanToken, ">");
    public static readonly SyntaxToken LessThanToken = new SyntaxToken(SyntaxKind.LessThanToken, "<");
    public static readonly SyntaxToken GreaterOrEqualsToken = new SyntaxToken(SyntaxKind.GreaterOrEqualsToken, ">=");
    public static readonly SyntaxToken LessThanEqualsToken = new SyntaxToken(SyntaxKind.LessThanEqualsToken, "<=");

    public static readonly SyntaxToken EqualsToken = new SyntaxToken(SyntaxKind.EqualsToken, "=");
    public static readonly SyntaxToken NotEqualsToken = new SyntaxToken(SyntaxKind.NotEqualsToken, "!=");
    public static readonly SyntaxToken PlusToken = new SyntaxToken(SyntaxKind.PlusToken, "+");
    public static readonly SyntaxToken MinusToken = new SyntaxToken(SyntaxKind.MinusToken, "-");
    public static readonly SyntaxToken PercentToken = new SyntaxToken(SyntaxKind.PercentToken, "%");
    public static readonly SyntaxToken SlashToken = new SyntaxToken(SyntaxKind.SlashToken, "/");
    public static readonly SyntaxToken StarToken = new SyntaxToken(SyntaxKind.StarToken, "*");
    public static readonly SyntaxToken CaretToken = new SyntaxToken(SyntaxKind.CaretToken, "^");
    public static readonly SyntaxToken ExclamationToken = new SyntaxToken(SyntaxKind.ExclamationToken, "!");

    public static readonly SyntaxToken CommaToken = new SyntaxToken(SyntaxKind.CommaToken, ",");
    public static readonly SyntaxToken ColonToken = new SyntaxToken(SyntaxKind.ColonToken, ":");
    public static readonly SyntaxToken SemicolonToken = new SyntaxToken(SyntaxKind.SemicolonToken, ";");

    public static SyntaxTrivia Whitespace(string text) => new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, text);
    public static readonly SyntaxTrivia LineFeed = new SyntaxTrivia(SyntaxKind.LineFeedTrivia, "\n");
    public static readonly SyntaxTrivia CarriageReturn = new SyntaxTrivia(SyntaxKind.CarriageReturnTrivia, "\r");
    public static readonly SyntaxTrivia CarriageReturnLineFeed = new SyntaxTrivia(SyntaxKind.CarriageReturnLineFeedTrivia, "\r\n");
    public static readonly SyntaxTrivia Space = new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, " ");
    public static readonly SyntaxTrivia Tab = new SyntaxTrivia(SyntaxKind.TabTrivia, "\t");
    public static SyntaxTrivia Comment(string text) => new SyntaxTrivia(SyntaxKind.CommentTrivia, text);


    //public static readonly SyntaxToken EndOfLine = new SyntaxToken(SyntaxKind.EndOfLineToken, "\n");
    public static readonly SyntaxToken EndOfFile = new SyntaxToken(SyntaxKind.EndOfFileToken, string.Empty);
}