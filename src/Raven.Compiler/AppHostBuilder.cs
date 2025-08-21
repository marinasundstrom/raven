using System.Reflection;
using System.Runtime.InteropServices;
using System.IO;

using Raven.CodeAnalysis;

namespace Raven;

static class AppHostBuilder
{
    public static void CreateAppHost(Compilation compilation, string outputPath, string targetFramework)
    {
        if (compilation.Options.OutputKind != OutputKind.ConsoleApplication)
            return;

        string? hostPath = null;
        string? hostBuilder = null;

        // TODO: Construct the appropriate path from target SDK.
        if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            hostPath = "/usr/local/share/dotnet/packs/Microsoft.NETCore.App.Host.osx-arm64/9.0.0/runtimes/osx-arm64/native/apphost";
            hostBuilder = "/usr/local/share/dotnet/sdk/9.0.101/Microsoft.NET.HostModel.dll";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            hostPath = "C:/Program Files/dotnet/packs/Microsoft.NETCore.App.Host.win-arm64/9.0.0/runtimes/win-arm64/native/apphost.exe";
            hostBuilder = "C:/Program Files/dotnet/sdk/9.0.100/Microsoft.NET.HostModel.dll";
        }

        if (hostPath is not null && hostBuilder is not null)
        {
            var assemblyName = Path.GetFileNameWithoutExtension(outputPath);
            var type = Assembly.LoadFile(hostBuilder).GetType("Microsoft.NET.HostModel.AppHost.HostWriter");
            type?.GetMethod("CreateAppHost")?.Invoke(null, new object?[]
            {
                hostPath,
                $"{assemblyName}.exe",
                Path.GetFileName(outputPath),
                false,
                null,
                false,
                false,
                null
            });
        }

        var runtimeConfigPath = Path.ChangeExtension(outputPath, ".runtimeconfig.json");
        var tfm = TargetFrameworkMoniker.ToTfm(targetFramework);
        var version = compilation.CoreAssembly.GetName().Version?.ToString() ?? "0.0.0";

        File.WriteAllText(runtimeConfigPath, $@"{{
  ""runtimeOptions"": {{
    ""tfm"": ""{tfm}"",
    ""framework"": {{
      ""name"": ""Microsoft.NETCore.App"",
      ""version"": ""{version}""
    }}
  }}
}}");
    }

}
