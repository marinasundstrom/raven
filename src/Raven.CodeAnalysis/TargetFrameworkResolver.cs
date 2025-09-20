using System.Runtime.InteropServices;
using System.Text.RegularExpressions;

namespace Raven.CodeAnalysis;

public static class TargetFrameworkResolver
{
    public static TargetFrameworkVersion ResolveVersion(string? tfmOrFull = null)
    {
        if (string.IsNullOrWhiteSpace(tfmOrFull))
        {
            return ResolveLatestInstalledVersion();
        }

        var parsed = TargetFrameworkMoniker.Parse(tfmOrFull);
        EnsureInstalled(parsed);
        return new TargetFrameworkVersion(parsed);
    }

    public static TargetFrameworkVersion ResolveLatestInstalledVersion(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var versions = GetInstalledVersions(sdkVersion, targetFramework, packId);
        var latest = versions
            .OrderByDescending(v => v.Moniker.Version)
            .ThenByDescending(v => v.Moniker.ToTfm(), StringComparer.OrdinalIgnoreCase)
            .FirstOrDefault();

        return latest ?? new TargetFrameworkVersion(new TargetFrameworkMoniker(FrameworkId.NetCoreApp, new Version(9, 0)));
    }

    public static TargetFrameworkVersion[] GetInstalledVersions(string? sdkVersion = null, string? targetFramework = null, string packId = "Microsoft.NETCore.App.Ref")
    {
        var list = new List<TargetFrameworkVersion>();

        foreach (var root in GetDotNetRoots())
        {
            var packRoot = Path.Combine(root, "packs", packId);
            if (!Directory.Exists(packRoot))
                continue;

            foreach (var versionDir in Directory.GetDirectories(packRoot))
            {
                var versionName = Path.GetFileName(versionDir)!;
                if (!Matches(versionName, sdkVersion))
                    continue;

                var refRoot = Path.Combine(versionDir, "ref");
                if (!Directory.Exists(refRoot))
                    continue;

                foreach (var tfmDir in Directory.GetDirectories(refRoot))
                {
                    var tfmName = Path.GetFileName(tfmDir)!;
                    if (!Matches(tfmName, targetFramework))
                        continue;

                    if (!TargetFrameworkMoniker.TryParse(tfmName, out var tfm))
                    {
                        tfm = TargetFrameworkMoniker.Parse(TargetFrameworkMonikerFromFolder(tfmName));
                    }

                    list.Add(new TargetFrameworkVersion(tfm!));
                }
            }
        }

        return list.ToArray();
    }

    private static bool Matches(string value, string? pattern)
    {
        if (string.IsNullOrWhiteSpace(pattern) || pattern == "*")
            return true;

        var re = WildcardToRegex(pattern);
        return Regex.IsMatch(value, re, RegexOptions.IgnoreCase);
    }

    private static IEnumerable<string> GetDotNetRoots()
    {
        var envRoots = new[]
        {
            Environment.GetEnvironmentVariable("DOTNET_ROOT"),
            Environment.GetEnvironmentVariable("DOTNET_ROOT(x86)")
        }.Where(s => !string.IsNullOrWhiteSpace(s));

        foreach (var r in envRoots!)
            if (Directory.Exists(r!))
                yield return r!;

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            var x64 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "dotnet");
            var x86 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), "dotnet");
            if (Directory.Exists(x64)) yield return x64;
            if (Directory.Exists(x86)) yield return x86;
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            const string ms = "/usr/local/share/dotnet";
            if (Directory.Exists(ms)) yield return ms;

            const string brew = "/opt/homebrew/opt/dotnet/libexec";
            if (Directory.Exists(brew)) yield return brew;
        }
        else
        {
            var candidates = new[]
            {
                "/usr/share/dotnet",
                "/usr/lib/dotnet",
                "/snap/dotnet-sdk/current",
            };

            foreach (var c in candidates)
                if (Directory.Exists(c))
                    yield return c;
        }
    }

    private static string WildcardToRegex(string pattern)
    {
        var escaped = Regex.Escape(pattern).Replace("\\*", ".*").Replace("\\?", ".");
        return $"^{escaped}$";
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
        EnsureInstalled(version.Moniker);
        return ReferenceAssemblyPaths.GetRuntimeDll(sdkVersion, version.Moniker.ToTfm(), packId);
    }
}

