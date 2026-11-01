namespace Raven.CodeAnalysis.Text;

public readonly record struct TextChangeRange(int NewLength, TextSpan Span)
{
    public IReadOnlyList<TextChangeRange> NoChanges => Array.Empty<TextChangeRange>();

    public override string ToString() => $"Span: {Span}, NewLength: {NewLength}";
}
