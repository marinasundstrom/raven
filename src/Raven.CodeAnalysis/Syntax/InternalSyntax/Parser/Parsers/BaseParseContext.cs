namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class BaseParseContext : ParseContext
{
    private SyntaxToken? _lastToken;
    private int _position;

    public BaseParseContext(Tokenizer tokenizer, int position = 0) : base()
    {
        Tokenizer = tokenizer;
        _position = position;
    }

    public Tokenizer Tokenizer { get; }

    public override int Position => _position;

    public override SyntaxToken LastToken => _lastToken ?? throw new InvalidOperationException("No token read yet.");

    public override SyntaxToken PeekToken(int index = 0) => Tokenizer.PeekToken(index);

    public override SyntaxToken ReadToken()
    {
        _lastToken = Tokenizer.ReadToken();
        _position += LastToken.FullWidth;
        return LastToken;
    }

    public override TextSpan GetStartOfLastToken()
    {
        return new TextSpan(Position - LastToken.FullWidth, 0);
    }

    public override TextSpan GetEndOfLastToken()
    {
        return new TextSpan(Position - LastToken.TrailingTrivia.Width, 0);
    }
}
