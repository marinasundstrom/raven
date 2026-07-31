namespace Raven.CodeAnalysis.Text;

public class TextLine
{
    private readonly SourceText _text;

    internal TextLine(SourceText text, int lineNumber, int start, int end, int endIncludingLineBreak)
    {
        _text = text;
        LineNumber = lineNumber;
        Start = start;
        End = end;
        EndIncludingLineBreak = endIncludingLineBreak;
    }

    public int LineNumber { get; }

    public int Start { get; }

    public int End { get; }

    public int EndIncludingLineBreak { get; }

    public TextSpan Span => TextSpan.FromBounds(Start, End);

    public TextSpan SpanIncludingLineBreak => TextSpan.FromBounds(Start, EndIncludingLineBreak);

    public override string ToString() => _text.ToString(Span);
}
