using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;

public struct TextChange
{
    private static List<TextChange>? _noChanges;

    public TextSpan Span { get; }
    public string NewText { get; }
    public IReadOnlyList<TextChange> NoChanges => _noChanges ??= new List<TextChange>();

    public TextChange(TextSpan span, string newText)
    {
        Span = span;
        NewText = newText;
    }

    public override string ToString() => $"Span: {Span}, NewText: {NewText}";
}