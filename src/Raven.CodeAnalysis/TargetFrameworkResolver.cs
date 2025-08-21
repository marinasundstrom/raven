namespace Raven.CodeAnalysis;

public static class TargetFrameworkResolver
{
    public static TargetFrameworkVersion ResolveVersion(string? tfmOrFull = null)
    {
        if (string.IsNullOrWhiteSpace(tfmOrFull))
        {
            return ResolveLatestVersion();
        }

        var parsed = TargetFrameworkMoniker.Parse(tfmOrFull);
        EnsureInstalled(parsed);
        return new TargetFrameworkVersion(parsed);
    }

    public static TargetFrameworkVersion ResolveLatestVersion(string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var dir = ReferenceAssemblyPaths.GetReferenceAssemblyDir(sdkVersion, targetFramework: null, packId);
        if (dir is null)
            return new TargetFrameworkVersion(new TargetFrameworkMoniker(FrameworkId.NetCoreApp, new Version(9, 0)));

        var tfmName = Path.GetFileName(dir);
        if (!TargetFrameworkMoniker.TryParse(tfmName, out var tfm))
        {
            tfm = TargetFrameworkMoniker.Parse(TargetFrameworkMonikerFromFolder(tfmName));
        }

        EnsureInstalled(tfm!);
        return new TargetFrameworkVersion(tfm!);
    }

    public static void EnsureInstalled(TargetFrameworkMoniker tfm)
    {
        if (ReferenceAssemblyPaths.GetReferenceAssemblyDir(targetFramework: tfm.ToTfm()) is null)
            throw new InvalidOperationException($"The target framework '{tfm.ToTfm()}' is not installed.");

        if (!string.IsNullOrEmpty(tfm.TargetPlatform))
        {
            // TODO: probe dotnet packs/workloads for platform targeting packs when needed.
        }
    }

    private static string TargetFrameworkMonikerFromFolder(string folder)
    {
        return folder;
    }

    public static string[] GetReferenceAssemblies(TargetFrameworkVersion version, string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        EnsureInstalled(version.Moniker);
        return ReferenceAssemblyPaths.GetReferenceAssemblyPaths(sdkVersion, version.Moniker.ToTfm(), packId);
    }

    public static string? GetDirectoryPath(TargetFrameworkVersion version, string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        EnsureInstalled(version.Moniker);
        return ReferenceAssemblyPaths.GetReferenceAssemblyDir(sdkVersion, version.Moniker.ToTfm(), packId);
    }

    public static string GetRuntimeDll(TargetFrameworkVersion version, string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var dir = GetDirectoryPath(version, sdkVersion, packId);
        return Path.Combine(dir!, "System.Runtime.dll");
    }
}

