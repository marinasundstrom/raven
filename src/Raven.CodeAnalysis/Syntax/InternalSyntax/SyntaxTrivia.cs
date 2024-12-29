namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxTrivia : GreenNode
{
    private SyntaxNode _structuredTrivia;

    public string Text { get; }

    public SyntaxTrivia(
        SyntaxKind kind,
        string text)
        : base(kind, 0, text.Length, text.Length)
    {
        Text = text;
    }

    public SyntaxTrivia(
        SyntaxNode node)
        : base(node.Kind, 0, node.Width, node.FullWidth)
    {
        _structuredTrivia = node;
        Text = string.Empty;
    }

    public bool HasStructuredTrivia => _structuredTrivia is not null;

    public SyntaxNode? GetStructuredTrivia() => _structuredTrivia;

    public override GreenNode GetSlot(int index) => throw new InvalidOperationException("SyntaxTrivia has no children.");

    public static explicit operator Syntax.SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia)
    {
        return new Syntax.SyntaxTrivia(trivia, default!);
    }
}