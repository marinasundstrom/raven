using System.Runtime.InteropServices;

namespace Raven;

static class ReferenceAssemblyPaths
{
    public static string[] GetReferenceAssemblyPaths()
    {
        var assemblyDir = GetReferenceAssemblyDir();

        return Directory.GetFiles(assemblyDir!, "*.dll");
    }

    private static string? GetReferenceAssemblyDir(string sdkVersion = "9.0.0", string targetFramework = "net9.0")
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
            //referencePath = $@"/usr/local/share/dotnet/packs/Microsoft.NETCore.App.Ref/{sdkVersion}/ref/{targetFramework}";
        }

        return referencePath;
    }
}