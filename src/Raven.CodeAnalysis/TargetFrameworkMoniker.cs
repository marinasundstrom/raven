using System;
using System.IO;

namespace Raven.CodeAnalysis;

public static class TargetFrameworkMoniker
{
    public static string Resolve(string? tfmOrFull = null)
    {
        if (string.IsNullOrWhiteSpace(tfmOrFull))
            return GetLatestFramework();

        if (tfmOrFull.Contains(",Version="))
        {
            EnsureInstalled(ToTfm(tfmOrFull));
            return tfmOrFull;
        }

        EnsureInstalled(tfmOrFull);
        return ToFrameworkString(tfmOrFull);
    }

    public static string GetLatestFramework()
    {
        var dir = ReferenceAssemblyPaths.GetReferenceAssemblyDir();
        if (dir is null)
            return ".NETCoreApp,Version=v9.0";

        var tfm = Path.GetFileName(dir);
        return ToFrameworkString(tfm!);
    }

    public static void EnsureInstalled(string tfm)
    {
        if (ReferenceAssemblyPaths.GetReferenceAssemblyDir(targetFramework: tfm) is null)
            throw new InvalidOperationException($"The target framework '{tfm}' is not installed.");
    }

    public static string ToFrameworkString(string tfm)
    {
        if (tfm.Contains(",Version="))
            return tfm;

        if (tfm.StartsWith("netstandard", StringComparison.OrdinalIgnoreCase))
            return ".NETStandard,Version=v" + tfm["netstandard".Length..];

        if (tfm.StartsWith("netcoreapp", StringComparison.OrdinalIgnoreCase))
            return ".NETCoreApp,Version=v" + tfm["netcoreapp".Length..];

        if (tfm.StartsWith("net", StringComparison.OrdinalIgnoreCase))
        {
            var ver = tfm[3..];
            if (ver.Length == 2 && char.IsDigit(ver[0]) && char.IsDigit(ver[1]))
                return $".NETFramework,Version=v{ver[0]}.{ver[1]}";

            return ".NETCoreApp,Version=v" + ver;
        }

        return tfm;
    }

    public static string ToTfm(string framework)
    {
        const string netCorePrefix = ".NETCoreApp,Version=v";
        const string netStandardPrefix = ".NETStandard,Version=v";
        const string netFrameworkPrefix = ".NETFramework,Version=v";

        if (framework.StartsWith(netCorePrefix, StringComparison.Ordinal))
            return "net" + framework[netCorePrefix.Length..];

        if (framework.StartsWith(netStandardPrefix, StringComparison.Ordinal))
            return "netstandard" + framework[netStandardPrefix.Length..];

        if (framework.StartsWith(netFrameworkPrefix, StringComparison.Ordinal))
            return "net" + framework[netFrameworkPrefix.Length..];

        return framework;
    }

    public static string GetDisplayName(string framework)
    {
        const string netCorePrefix = ".NETCoreApp,Version=v";
        const string netStandardPrefix = ".NETStandard,Version=v";
        const string netFrameworkPrefix = ".NETFramework,Version=v";

        if (framework.StartsWith(netCorePrefix, StringComparison.Ordinal))
            return $".NET {framework[netCorePrefix.Length..]}";

        if (framework.StartsWith(netStandardPrefix, StringComparison.Ordinal))
            return $".NET Standard {framework[netStandardPrefix.Length..]}";

        if (framework.StartsWith(netFrameworkPrefix, StringComparison.Ordinal))
            return $".NET Framework {framework[netFrameworkPrefix.Length..]}";

        return framework;
    }
}

