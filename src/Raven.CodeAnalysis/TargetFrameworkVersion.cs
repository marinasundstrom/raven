namespace Raven.CodeAnalysis;

public sealed record TargetFrameworkVersion(TargetFrameworkMoniker Moniker)
{
    public string ToFrameworkString() => Moniker.ToFrameworkString();
    public string ToTfm() => Moniker.ToTfm();
    public string GetDisplayName() => Moniker.GetDisplayName();
}
