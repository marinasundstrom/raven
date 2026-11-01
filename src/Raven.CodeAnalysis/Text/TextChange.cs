namespace Raven.CodeAnalysis.Text;

public readonly record struct TextChange(TextSpan Span, string NewText)
{
    public IReadOnlyList<TextChange> NoChanges => Array.Empty<TextChange>();

    public override string ToString() => $"Span: {Span}, NewText: {NewText}";
}
