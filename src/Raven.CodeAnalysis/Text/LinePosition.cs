namespace Raven.CodeAnalysis.Text;

public struct LinePosition : IComparable<LinePosition>, IEquatable<LinePosition>
{
    public static LinePosition Zero { get; } = new LinePosition(0, 0);

    public LinePosition(int line, int character)
    {
        if (line < 0)
            throw new ArgumentOutOfRangeException(nameof(line), "Line must be non-negative.");
        if (character < 0)
            throw new ArgumentOutOfRangeException(nameof(character), "Character must be non-negative.");

        Line = line;
        Character = character;
    }

    public int Character { get; }
    public int Line { get; }

    // Compare two LinePosition instances
    public int CompareTo(LinePosition other)
    {
        // Compare lines first
        if (Line < other.Line)
            return -1;
        if (Line > other.Line)
            return 1;

        // If lines are equal, compare characters
        if (Character < other.Character)
            return -1;
        if (Character > other.Character)
            return 1;

        // Line and character are equal
        return 0;
    }

    // Check if two LinePosition instances are equal
    public bool Equals(LinePosition other)
    {
        return Line == other.Line && Character == other.Character;
    }

    // Override Equals for object comparison
    public override bool Equals(object obj)
    {
        return obj is LinePosition other && Equals(other);
    }

    // Override GetHashCode
    public override int GetHashCode()
    {
        // Combine line and character into a hash
        return HashCode.Combine(Line, Character);
    }

    // Override ToString for better debugging and display
    public override string ToString()
    {
        return $"Line {Line}, Character {Character}";
    }

    // Define equality operators
    public static bool operator ==(LinePosition left, LinePosition right)
    {
        return left.Equals(right);
    }

    public static bool operator !=(LinePosition left, LinePosition right)
    {
        return !left.Equals(right);
    }

    // Optional: Define comparison operators
    public static bool operator <(LinePosition left, LinePosition right)
    {
        return left.CompareTo(right) < 0;
    }

    public static bool operator >(LinePosition left, LinePosition right)
    {
        return left.CompareTo(right) > 0;
    }

    public static bool operator <=(LinePosition left, LinePosition right)
    {
        return left.CompareTo(right) <= 0;
    }

    public static bool operator >=(LinePosition left, LinePosition right)
    {
        return left.CompareTo(right) >= 0;
    }
}