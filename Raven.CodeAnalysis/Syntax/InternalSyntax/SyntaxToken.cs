namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxToken : GreenNode
{
    public string Text { get; }
    public SyntaxTriviaList LeadingTrivia { get; }
    public SyntaxTriviaList TrailingTrivia { get; }

    public SyntaxToken(
        SyntaxKind kind,
        string text,
        SyntaxTriviaList leadingTrivia = null,
        SyntaxTriviaList trailingTrivia = null,
        IEnumerable<DiagnosticInfo> diagnostics = null,
        int startPosition = 0)
        : base(kind, 0, (leadingTrivia?.FullWidth ?? 0) + text.Length + (trailingTrivia?.FullWidth ?? 0), diagnostics, startPosition)
    {
        Text = text;
        LeadingTrivia = leadingTrivia ?? new SyntaxTriviaList();
        TrailingTrivia = trailingTrivia ?? new SyntaxTriviaList();
    }

    public override GreenNode GetSlot(int index) => throw new InvalidOperationException("SyntaxToken has no children.");
}