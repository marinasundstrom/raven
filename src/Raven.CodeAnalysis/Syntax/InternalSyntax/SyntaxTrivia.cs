namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxTrivia : GreenNode
{
    public string Text { get; }

    public SyntaxTrivia(
        SyntaxKind kind,
        string text)
        : base(kind, 0, text.Length, text.Length)
    {
        Text = text;
    }

    public override GreenNode GetSlot(int index) => throw new InvalidOperationException("SyntaxTrivia has no children.");

    public static explicit operator Syntax.SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia)
    {
        return new Syntax.SyntaxTrivia(trivia, default!);
    }
}