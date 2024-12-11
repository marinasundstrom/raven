
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class SourceText
{
    private readonly string _text;
    private readonly List<int> _lineStarts;

    public SourceText(string text)
    {
        _text = text ?? throw new ArgumentNullException(nameof(text));
        _lineStarts = ComputeLineStarts(_text);
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
        return new SourceText(updatedText);
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
}