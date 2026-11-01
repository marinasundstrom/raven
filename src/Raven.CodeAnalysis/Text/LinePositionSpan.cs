namespace Raven.CodeAnalysis.Text;

public readonly record struct LinePositionSpan(LinePosition Start, LinePosition End)
{
    public override string ToString() => $"LinePositionSpan {{ Start = {Start}, End = {End} }}";
}
