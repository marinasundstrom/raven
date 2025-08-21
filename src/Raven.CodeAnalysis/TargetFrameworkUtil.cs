namespace Raven.CodeAnalysis;

public static class TargetFrameworkUtil
{
    public static string Resolve(string? tfmOrFull = null)
        => TargetFrameworkResolver.Resolve(tfmOrFull);

    public static string GetLatestFramework()
        => TargetFrameworkResolver.GetLatestFrameworkTfm().ToFrameworkString();

    public static void EnsureInstalled(string tfm)
        => TargetFrameworkResolver.EnsureInstalled(TargetFrameworkMoniker.Parse(tfm));

    public static string ToFrameworkString(string tfm)
        => TargetFrameworkMoniker.Parse(tfm).ToFrameworkString();

    public static string ToTfm(string framework)
        => TargetFrameworkMoniker.Parse(framework).ToTfm();

    public static string GetDisplayName(string frameworkOrTfm)
        => TargetFrameworkMoniker.Parse(frameworkOrTfm).GetDisplayName();
}
