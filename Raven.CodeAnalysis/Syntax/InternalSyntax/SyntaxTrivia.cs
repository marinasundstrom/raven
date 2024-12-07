namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxTrivia : GreenNode
{
    public string Text { get; }

    public SyntaxTrivia(
        SyntaxKind kind,
        string text,
        IEnumerable<DiagnosticInfo> diagnostics = null,
        int startPosition = 0)
        : base(kind, 0, text.Length, diagnostics, startPosition)
    {
        Text = text;
    }

    public override GreenNode GetSlot(int index) => throw new InvalidOperationException("SyntaxTrivia has no children.");
}