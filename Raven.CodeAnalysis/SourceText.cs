
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class SourceText
{
    private readonly string _text;
    private readonly List<int> _lineStarts;

    private SourceText(string text)
    {
        _text = text ?? throw new ArgumentNullException(nameof(text));
        _lineStarts = ComputeLineStarts(_text);
    }

    public static SourceText From(string text)
    {
        return new SourceText(text);
    }

    private List<int> ComputeLineStarts(string text)
    {
        var lineStarts = new List<int> { 0 };
        for (int i = 0; i < text.Length; i++)
        {
            if (text[i] == '\n' || (text[i] == '\r' && (i + 1 == text.Length || text[i + 1] != '\n')))
            {
                lineStarts.Add(i + 1);
            }
        }
        return lineStarts;
    }


    public SourceText WithChange(int position, string newText)
    {
        if (position < 0 || position > _text.Length)
            throw new ArgumentOutOfRangeException(nameof(position));

        // Apply the change
        string updatedText = _text.Substring(0, position) + newText + _text.Substring(position);
        return From(updatedText);
    }
    
    public SourceText WithChange(TextChange change)
    {
        if (change.Span.Start < 0 || change.Span.Start > _text.Length ||
            change.Span.End < 0 || change.Span.End > _text.Length)
            throw new ArgumentOutOfRangeException(nameof(change), "The span is out of bounds.");

        // Apply the change
        string updatedText = _text.Substring(0, change.Span.Start) +
                             change.NewText +
                             _text.Substring(change.Span.End);

        // Return a new SourceText instance with the updated text
        return SourceText.From(updatedText);
    }

    public (int line, int column) GetLineAndColumn(int position)
    {
        if (position < 0 || position > _text.Length)
            throw new ArgumentOutOfRangeException(nameof(position));

        int line = _lineStarts.BinarySearch(position);
        if (line < 0)
            line = ~line - 1;

        int column = position - _lineStarts[line];
        return (line + 1, column + 1); // 1-based indexing
    }

    public (int line, int column) GetLineAndColumn(TextSpan span) => GetLineAndColumn(span.Start + span.Length);

    public string GetSubstring(TextSpan span) => GetSubstring(span.Start, span.Length);

    public string GetSubstring(int start, int length) => _text.Substring(start, length);

    public TextReader GetTextReader()
    {
        return new StringReader(_text);
    }
    
    public IReadOnlyList<TextChange> GetChangeRanges(SourceText oldText)
    {
        if (oldText == null)
            throw new ArgumentNullException(nameof(oldText));

        var oldTextString = oldText._text;
        var newTextString = this._text;

        int oldLength = oldTextString.Length;
        int newLength = newTextString.Length;

        int start = 0;
        while (start < oldLength && start < newLength && oldTextString[start] == newTextString[start])
        {
            start++;
        }

        if (start == oldLength && start == newLength)
        {
            // No changes
            return Array.Empty<TextChange>();
        }

        int oldEnd = oldLength - 1;
        int newEnd = newLength - 1;

        while (oldEnd >= start && newEnd >= start && oldTextString[oldEnd] == newTextString[newEnd])
        {
            oldEnd--;
            newEnd--;
        }

        // Calculate the spans and the new text
        int oldSpanStart = start;
        int oldSpanLength = oldEnd - start + 1;
        int newSpanStart = start;
        int newSpanLength = newEnd - start + 1;

        string changedText = newTextString.Substring(newSpanStart, newSpanLength);

        return new List<TextChange>
        {
            new TextChange(new TextSpan(oldSpanStart, oldSpanLength), changedText)
        };
    }
}