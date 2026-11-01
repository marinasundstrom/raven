namespace Raven.CodeAnalysis.Text;

public readonly record struct FileLinePositionSpan(
    string Path,
    LinePosition StartLinePosition,
    LinePosition EndLinePosition)
{
    public override string ToString() => $"FileLinePositionSpan {{ Path = {Path}, StartLinePosition = {StartLinePosition}, EndLinePosition = {EndLinePosition} }}";
}
