namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxTrivia : GreenNode
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
}