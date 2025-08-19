

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal static partial class SyntaxFactory
{
    public static SyntaxToken MissingToken(SyntaxKind kind) => SyntaxToken.Missing(kind);

    public static SyntaxToken Token(SyntaxKind kind) => new SyntaxToken(kind, string.Empty);

    public static SyntaxToken IdentifierToken(string text) => new SyntaxToken(SyntaxKind.IdentifierToken, text);
    public static SyntaxToken NumericLiteral(int value) => new SyntaxToken(SyntaxKind.NumericLiteralToken, value.ToString(), value, value.ToString().Length);

    public static SyntaxTrivia Whitespace(string text) => new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, text);
    public static readonly SyntaxTrivia LineFeed = new SyntaxTrivia(SyntaxKind.LineFeedTrivia, "\n");
    public static readonly SyntaxTrivia CarriageReturn = new SyntaxTrivia(SyntaxKind.CarriageReturnTrivia, "\r");
    public static readonly SyntaxTrivia CarriageReturnLineFeed = new SyntaxTrivia(SyntaxKind.CarriageReturnLineFeedTrivia, "\r\n");
    public static readonly SyntaxTrivia Space = new SyntaxTrivia(SyntaxKind.WhitespaceTrivia, " ");
    public static readonly SyntaxTrivia Tab = new SyntaxTrivia(SyntaxKind.TabTrivia, "\t");
    public static SyntaxTrivia SingleLineComment(string text) => new SyntaxTrivia(SyntaxKind.SingleLineCommentTrivia, text);
    public static SkippedTokensTrivia SkippedTokensTrivia(SyntaxTokenList tokens) => new SkippedTokensTrivia(tokens.Green, []);

    public static SyntaxList TokenList(params IEnumerable<SyntaxToken> tokens) => new SyntaxList(tokens.ToArray());

    public static SyntaxTrivia StructuredTrivia(SyntaxNode node) => new SyntaxTrivia(node);
}