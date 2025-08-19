using System;
using System.IO;
using System.Runtime.InteropServices;

namespace Raven.CodeAnalysis;

public static class ReferenceAssemblyPaths
{
    public static string[] GetReferenceAssemblyPaths(string sdkVersion = "9.0.7", string targetFramework = "net9.0")
    {
        var assemblyDir = GetReferenceAssemblyDir(sdkVersion, targetFramework);
        if (assemblyDir is null || !Directory.Exists(assemblyDir))
            return Array.Empty<string>();

        return Directory.GetFiles(assemblyDir, "*.dll");
    }

    public static string? GetReferenceAssemblyDir(string sdkVersion = "9.0.7", string targetFramework = "net9.0")
    {
        string? referencePath = null;

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            referencePath = $@"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Ref\{sdkVersion}\ref\{targetFramework}";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            referencePath = $@"/usr/local/share/dotnet/packs/Microsoft.NETCore.App.Ref/{sdkVersion}/ref/{targetFramework}";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            var root = Environment.GetEnvironmentVariable("DOTNET_ROOT") ?? "/usr/lib/dotnet";
            referencePath = Path.Combine(root, "packs", "Microsoft.NETCore.App.Ref", sdkVersion, "ref", targetFramework);
        }

        return referencePath;
    }
}