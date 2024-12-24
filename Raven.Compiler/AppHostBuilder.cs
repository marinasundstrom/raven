using System.Reflection;
using System.Runtime.InteropServices;

using Raven.CodeAnalysis;

namespace Raven;

static class AppHostBuilder
{
    public static void CreateAppHost(Compilation compilation)
    {
        if (compilation.Options.OutputKind == OutputKind.ConsoleApplication)
        {
            string? hostPath = null;
            string? hostBuilder = null;

            if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            {
                hostPath = @"/usr/local/share/dotnet/packs/Microsoft.NETCore.App.Host.osx-arm64/9.0.0/runtimes/osx-arm64/native/apphost";
                hostBuilder = @"/usr/local/share/dotnet/sdk/9.0.100/Microsoft.NET.HostModel.dll";
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                hostPath = @"C:\Program Files\dotnet\packs\Microsoft.NETCore.App.Host.win-arm64\9.0.0\runtimes\win-arm64\native\apphost.exe";
                hostBuilder = @"C:\Program Files\dotnet\sdk\9.0.100\Microsoft.NET.HostModel.dll";
            }

            string assemblyName = compilation.AssemblyName;

            var type = Assembly.LoadFile(hostBuilder!).GetType("Microsoft.NET.HostModel.AppHost.HostWriter");

            type?.GetMethod("CreateAppHost")
                ?.Invoke(null, [
                    hostPath!,
                $"{assemblyName}.exe",
                $"{assemblyName}.dll",
                false,
                null,
                false,
                false,
                null]);

            File.WriteAllText($"{assemblyName}.runtimeconfig.json",
            """
            {
                "runtimeOptions": {
                    "tfm": "net9.0",
                    "framework": {
                        "name": "Microsoft.NETCore.App",
                        "version": "9.0.0"
                    }
                }
            }
            """);
        }
    }
}

public static class TargetFrameworks
{
    public static readonly IEnumerable<string> Versions = [
        ".NETStandard,Version=v2.0",
        ".NETStandard,Version=v2.1",
        ".NETFramework,Version=v7.8",
        ".NETCoreApp,Version=v6.0",
        ".NETCoreApp,Version=v7.0",
        ".NETCoreApp,Version=v8.0",
        ".NETCoreApp,Version=v9.0"
    ];
}