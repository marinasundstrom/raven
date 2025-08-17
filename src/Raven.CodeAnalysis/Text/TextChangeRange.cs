using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;

public struct TextChangeRange
{
    private static List<TextChangeRange>? _noChangesRange;

    public int NewLength { get; }
    public TextSpan Span { get; }
    public IReadOnlyList<TextChangeRange> NoChanges => _noChangesRange ??= new List<TextChangeRange>();

    public TextChangeRange(int maxLength, TextSpan span)
    {
        NewLength = maxLength;
        Span = span;
    }

    public override string ToString() => $"Span: {Span}, NewLength: {NewLength}";
}