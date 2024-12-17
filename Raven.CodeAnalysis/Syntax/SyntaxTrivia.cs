namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTrivia
{
    internal readonly InternalSyntax.SyntaxTrivia Green;
    private readonly SyntaxToken? _token;

    public SyntaxKind Kind => Green.Kind;
    public string Text => Green.Text;

    public SyntaxToken? Token => _token;

    internal SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia, SyntaxToken parent)
    {
        Green = trivia ?? throw new ArgumentNullException(nameof(trivia));
        _token = parent;
    }

    public SyntaxTrivia(SyntaxKind kind)
    {
        Green = new InternalSyntax.SyntaxTrivia(kind, SyntaxFacts.GetSyntaxTokenText(kind)!);
        _token = null!;
    }

    public override string ToString() => Text;
}

public static partial class SyntaxFactory
{
    public static SyntaxTrivia Whitespace(string text) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.Whitespace(text);
    public static readonly SyntaxTrivia LineFeed = (SyntaxTrivia)InternalSyntax.SyntaxFactory.LineFeed;
    public static readonly SyntaxTrivia CarriageReturn = (SyntaxTrivia)InternalSyntax.SyntaxFactory.CarriageReturn;
    public static readonly SyntaxTrivia CarriageReturnLineFeed = (SyntaxTrivia)InternalSyntax.SyntaxFactory.CarriageReturnLineFeed;
    public static readonly SyntaxTrivia Space = (SyntaxTrivia)InternalSyntax.SyntaxFactory.Space;
    public static readonly SyntaxTrivia Tab = (SyntaxTrivia)InternalSyntax.SyntaxFactory.Tab;
    public static SyntaxTrivia Comment(string text) => (SyntaxTrivia)InternalSyntax.SyntaxFactory.Comment(text);
}