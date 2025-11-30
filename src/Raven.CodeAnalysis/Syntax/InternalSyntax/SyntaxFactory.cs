

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal static partial class SyntaxFactory
{
    public static SyntaxToken MissingToken(SyntaxKind kind) => SyntaxToken.Missing(kind);

    public static SyntaxToken Token(SyntaxKind kind) => new(kind, SyntaxFacts.GetSyntaxTokenText(kind) ?? string.Empty);

    public static SyntaxToken IdentifierToken(string text) => new(SyntaxKind.IdentifierToken, text);
    public static SyntaxToken Literal(int value) => new(SyntaxKind.NumericLiteralToken, value.ToString(), value, value.ToString().Length);
    public static SyntaxToken Literal(string value) => new(SyntaxKind.StringLiteralToken, value, value, value.Length);

    public static SyntaxToken Literal(string text, int value) => new(SyntaxKind.NumericLiteralToken, text, value, text.Length);
    public static SyntaxToken Literal(string text, string value) => new(SyntaxKind.StringLiteralToken, text, value, text.Length);


    public static SyntaxTrivia Whitespace(string text) => new(SyntaxKind.WhitespaceTrivia, text);
    public static readonly SyntaxTrivia LineFeed = new(SyntaxKind.LineFeedTrivia, "\n");
    public static readonly SyntaxTrivia CarriageReturn = new(SyntaxKind.CarriageReturnTrivia, "\r");
    public static readonly SyntaxTrivia CarriageReturnLineFeed = new(SyntaxKind.CarriageReturnLineFeedTrivia, "\r\n");
    public static readonly SyntaxTrivia Space = new(SyntaxKind.WhitespaceTrivia, " ");
    public static readonly SyntaxTrivia Tab = new(SyntaxKind.TabTrivia, "\t");

    public static readonly SyntaxToken NewLineToken = new(SyntaxKind.NewLineToken, "");
    public static readonly SyntaxToken EndOfFileToken = new(SyntaxKind.EndOfFileToken, "");

    public static SyntaxTrivia SingleLineComment(string text) => new(SyntaxKind.SingleLineCommentTrivia, text);
    public static SkippedTokensTrivia SkippedTokensTrivia(SyntaxTokenList tokens) => new(tokens.Green, []);

    public static SyntaxList TokenList(params IEnumerable<SyntaxToken> tokens) => new(tokens.ToArray());

    public static SyntaxTrivia StructuredTrivia(SyntaxNode node) => new(node);
}
