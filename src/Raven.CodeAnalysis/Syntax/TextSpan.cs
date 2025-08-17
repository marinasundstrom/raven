using System.Diagnostics;

namespace Raven.CodeAnalysis.Syntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public struct TextSpan : IEquatable<TextSpan>, IComparable<TextSpan>
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
        return $"[{Start}..{End}]";
    }

    public override string ToString()
    {
        return $"[{Start}..{End}]";
    }

    public int CompareTo(TextSpan other)
    {
        // 1. Compare Start positions
        int startComparison = Start.CompareTo(other.Start);
        if (startComparison != 0)
            return startComparison;

        // 2. If Starts are equal, compare End positions
        //    (i.e., the span that ends first is "less")
        return End.CompareTo(other.End);
    }

    // Equality (IEquatable<TextSpan>)
    public bool Equals(TextSpan other)
    {
        // Two spans are equal if start and length match
        return Start == other.Start && Length == other.Length;
    }

    // Override Equals(object)
    public override bool Equals(object? obj)
    {
        if (obj is TextSpan other)
            return Equals(other);

        return false;
    }

    // Override GetHashCode()
    public override int GetHashCode()
    {
        // Combine Start and Length into a single hash code
        return HashCode.Combine(Start, Length);
    }

    // Optional convenience operators
    public static bool operator ==(TextSpan? left, TextSpan? right)
    {
        if (left is null)
            return right is null;

        return left.Equals(right);
    }

    public static bool operator !=(TextSpan? left, TextSpan? right)
    {
        return !(left == right);
    }
}