namespace Raven.CodeAnalysis;

public static class TargetFrameworkResolver
{
    /// <summary>
    /// Resolve a user-provided string (TFM or full framework string) to a parsed TFM,
    /// validate it, and return the *full framework string* (for metadata).
    /// </summary>
    public static string Resolve(string? tfmOrFull = null)
    {
        if (string.IsNullOrWhiteSpace(tfmOrFull))
        {
            var latest = GetLatestFrameworkTfm();
            EnsureInstalled(latest);
            return latest.ToFrameworkString();
        }

        var parsed = TargetFrameworkMoniker.Parse(tfmOrFull);
        EnsureInstalled(parsed);
        return parsed.ToFrameworkString();
    }

    public static TargetFrameworkMoniker GetLatestFrameworkTfm()
    {
        // Honor your existing logic
        var dir = ReferenceAssemblyPaths.GetReferenceAssemblyDir();
        if (dir is null)
            return new TargetFrameworkMoniker(FrameworkId.NetCoreApp, new Version(9, 0)); // default fallback

        var tfmName = Path.GetFileName(dir);
        if (!TargetFrameworkMoniker.TryParse(tfmName, out var tfm))
        {
            // Try interpreting folder names like "net8.0"
            tfm = TargetFrameworkMoniker.Parse(TargetFrameworkMonikerFromFolder(tfmName));
        }

        return tfm!;
    }

    public static void EnsureInstalled(TargetFrameworkMoniker tfm)
    {
        // Base reference assemblies / targeting pack
        if (ReferenceAssemblyPaths.GetReferenceAssemblyDir(targetFramework: tfm.ToTfm()) is null)
            throw new InvalidOperationException($"The target framework '{tfm.ToTfm()}' is not installed.");

        // Platform-specific workloads (optional, stubbed for future):
        if (!string.IsNullOrEmpty(tfm.TargetPlatform))
        {
            // TODO: probe dotnet packs/workloads for platform targeting packs when needed.
            // For now, keep it permissive; your ReferenceAssemblyPaths likely handles netX.Y base packs.
        }
    }

    private static string TargetFrameworkMonikerFromFolder(string folder)
    {
        // e.g., "net8.0" -> same; ".NETCoreApp,Version=v8.0" folder wouldn't normally appear here
        return folder;
    }

    /// <summary>
    /// Returns full paths to all reference assemblies for the resolved SDK version and target framework.
    /// </summary>
    public static string[] GetReferenceAssemblyPaths(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var tfm = TargetFrameworkMoniker.Parse(Resolve(targetFramework)).ToTfm();
        return ReferenceAssemblyPaths.GetReferenceAssemblyPaths(sdkVersion, tfm, packId);
    }

    /// <summary>
    /// Resolves the reference assemblies directory for the chosen SDK version and target framework.
    /// </summary>
    public static string? GetReferenceAssemblyDir(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var tfm = TargetFrameworkMoniker.Parse(Resolve(targetFramework)).ToTfm();
        return ReferenceAssemblyPaths.GetReferenceAssemblyDir(sdkVersion, tfm, packId);
    }

    /// <summary>
    /// Gets the path to System.Runtime.dll for the resolved SDK version and target framework.
    /// </summary>
    public static string GetRuntimeDll(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var dir = GetReferenceAssemblyDir(sdkVersion, targetFramework, packId);
        return Path.Combine(dir!, "System.Runtime.dll");
    }
}
