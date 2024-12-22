using System.Diagnostics;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public class TextSpan : IComparable<TextSpan>
{
    public int Start { get; }

    public TextSpan(int start, int length)
    {
        if (start < 0 || length < 0)
            throw new ArgumentOutOfRangeException();

        Start = start;
        Length = length;
    }

    public int Length { get; }

    public int End => Start + Length;

    private string GetDebuggerDisplay()
    {
        return $"{Start}..{End}";
    }

    public override string ToString()
    {
        return $"{Start}..{End}";
    }

    public int CompareTo(TextSpan? other)
    {
        // Compare by Span Start
        return Start.CompareTo(other?.Start);
    }
}