namespace Raven.CodeAnalysis.Text;

public struct LinePositionSpan
{
    public LinePositionSpan(LinePosition start, LinePosition end)
    {
        Start = start;
        End = end;
    }

    public LinePosition Start { get; }
    public LinePosition End { get; }
}