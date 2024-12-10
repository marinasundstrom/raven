namespace Raven.CodeAnalysis.Syntax;

public struct SyntaxTrivia
{
    internal readonly InternalSyntax.SyntaxTrivia Green;
    private readonly SyntaxToken? _token;

    public SyntaxKind Kind => Green.Kind;
    public string Text => Green.Text;
    public IEnumerable<DiagnosticInfo> Diagnostics => Green.Diagnostics;

    public SyntaxToken? Token => _token;

    public SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia, SyntaxToken parent)
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

    public static explicit operator SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia)
    {
        return new SyntaxTrivia(trivia, default!);
    }

    public void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitTrivia(this);
    }

    public TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
    {
        return visitor.VisitTrivia(this);
    }
}