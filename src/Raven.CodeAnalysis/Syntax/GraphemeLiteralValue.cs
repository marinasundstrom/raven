namespace Raven.CodeAnalysis.Syntax;

internal sealed record GraphemeLiteralValue(string Text)
{
    public override string ToString() => Text;
}
