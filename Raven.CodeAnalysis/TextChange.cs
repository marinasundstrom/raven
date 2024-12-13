using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public struct TextChange
{
    public TextSpan Span { get; }
    public string NewText { get; }

    public TextChange(TextSpan span, string newText)
    {
        Span = span;
        NewText = newText;
    }

    public override string ToString() => $"Span: {Span}, NewText: {NewText}";
}