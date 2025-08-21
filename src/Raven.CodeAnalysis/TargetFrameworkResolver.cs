namespace Raven.CodeAnalysis;

public static class TargetFrameworkResolver
{
    public static TargetFrameworkMoniker GetVersion(string? tfmOrFull = null)
    {
        if (string.IsNullOrWhiteSpace(tfmOrFull))
        {
            return GetLatestVersion();
        }

        var parsed = TargetFrameworkMoniker.Parse(tfmOrFull);
        EnsureInstalled(parsed);
        return parsed;
    }

    public static TargetFrameworkMoniker GetLatestVersion(string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var dir = ReferenceAssemblyPaths.GetReferenceAssemblyDir(sdkVersion, targetFramework: null, packId);
        if (dir is null)
            return new TargetFrameworkMoniker(FrameworkId.NetCoreApp, new Version(9, 0));

        var tfmName = Path.GetFileName(dir);
        if (!TargetFrameworkMoniker.TryParse(tfmName, out var tfm))
        {
            tfm = TargetFrameworkMoniker.Parse(TargetFrameworkMonikerFromFolder(tfmName));
        }

        EnsureInstalled(tfm!);
        return tfm!;
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

    public static string[] GetReferenceAssemblies(TargetFrameworkMoniker version, string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        EnsureInstalled(version);
        return ReferenceAssemblyPaths.GetReferenceAssemblyPaths(sdkVersion, version.ToTfm(), packId);
    }

    public static string? GetDirectoryPath(TargetFrameworkMoniker version, string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        EnsureInstalled(version);
        return ReferenceAssemblyPaths.GetReferenceAssemblyDir(sdkVersion, version.ToTfm(), packId);
    }

    public static string GetRuntimeDll(TargetFrameworkMoniker version, string? sdkVersion = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var dir = GetDirectoryPath(version, sdkVersion, packId);
        return Path.Combine(dir!, "System.Runtime.dll");
    }
}

