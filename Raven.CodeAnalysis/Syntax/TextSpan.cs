using System.Diagnostics;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public class TextSpan
{
    public int Start { get; }

    public TextSpan(int start, int length)
    {
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
}