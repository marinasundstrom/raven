
using System.Collections;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Text;

public class TextLineCollection : IEnumerable<TextLine>
{
    private readonly ImmutableArray<TextLine> _lines;

    internal TextLineCollection(SourceText text, IReadOnlyList<int> lineStarts)
    {
        var builder = ImmutableArray.CreateBuilder<TextLine>(lineStarts.Count);

        for (var index = 0; index < lineStarts.Count; index++)
        {
            var start = lineStarts[index];
            var endIncludingLineBreak = index + 1 < lineStarts.Count
                ? lineStarts[index + 1]
                : text.Length;
            var end = start + text.GetLineLength(index);
            builder.Add(new TextLine(text, index, start, end, endIncludingLineBreak));
        }

        _lines = builder.MoveToImmutable();
    }

    public int Count => _lines.Length;

    public TextLine this[int index] => _lines[index];

    public IEnumerator<TextLine> GetEnumerator() => ((IEnumerable<TextLine>)_lines).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}
