namespace Raven.CodeAnalysis.Syntax;

public static class TextSpanExtensions
{
    // Checks if this TextSpan fully contains another TextSpan
    public static bool Contains(this TextSpan outerSpan, TextSpan innerSpan)
    {
        return outerSpan.Start <= innerSpan.Start && outerSpan.End >= innerSpan.End;
    }

    // Checks if this TextSpan contains a specific position
    public static bool Contains(this TextSpan span, int position)
    {
        return position >= span.Start && position < span.End;
    }

    // Checks if this TextSpan intersects with the specified TextSpan.
    public static bool IntersectsWith(this TextSpan span, TextSpan otherSpan)
    {
        return span.Start < otherSpan.End && otherSpan.Start < span.End;
    }
}