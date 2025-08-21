using System;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;

using Microsoft.NET.HostModel.AppHost;

using Raven.CodeAnalysis;

namespace Raven;

static class AppHostBuilder
{
    public static void CreateAppHost(Compilation compilation, string outputPath, string targetFramework)
    {
        if (compilation.Options.OutputKind != OutputKind.ConsoleApplication)
            return;

        // 1) Resolve RID
        var rid = GetDefaultRid();

        // 2) Resolve dotnet root
        var dotnetRoot = GetDotnetRoot();
        if (dotnetRoot is null)
            throw new InvalidOperationException("Unable to locate DOTNET_ROOT (and no standard install path was found).");

        // 3) Resolve runtime (host pack) version (product version, 3 segments)
        var coreAsmVer = compilation.CoreAssembly.GetName().Version ?? new Version(0, 0, 0, 0);
        var desiredMajorMinor = new Version(coreAsmVer.Major, coreAsmVer.Minor);

        // Try exact "x.y.z" from core assembly (drop 4th segment)
        var desiredVersion3 = new Version(coreAsmVer.Major, coreAsmVer.Minor, coreAsmVer.Build >= 0 ? coreAsmVer.Build : 0);
        var desiredVersionStr = desiredVersion3.ToString(3);

        // 4) Build path to apphost template in the host pack (with probing)
        string packsRoot = Path.Combine(dotnetRoot, "packs", $"Microsoft.NETCore.App.Host.{rid}");
        if (!Directory.Exists(packsRoot))
            throw new DirectoryNotFoundException($"Host pack root not found: {packsRoot}");

        string? pickVersionDir = null;

        // exact match first
        var exactDir = Path.Combine(packsRoot, desiredVersionStr);
        if (Directory.Exists(exactDir)) pickVersionDir = exactDir;

        if (pickVersionDir is null)
        {
            // enumerate available versions and choose best
            Version? best = null;
            foreach (var dir in Directory.EnumerateDirectories(packsRoot))
            {
                var name = Path.GetFileName(dir);
                if (Version.TryParse(name.Split('-')[0], out var v)) // ignore -preview labels
                {
                    // prefer same major.minor; otherwise take highest
                    bool preferredBand = v.Major == desiredMajorMinor.Major && v.Minor == desiredMajorMinor.Minor;
                    bool currentBand = best is not null && best.Major == desiredMajorMinor.Major && best.Minor == desiredMajorMinor.Minor;

                    if (best is null ||
                        (preferredBand && !currentBand) ||
                        (preferredBand == currentBand && v > best))
                    {
                        best = v;
                        pickVersionDir = dir;
                    }
                }
            }
        }

        if (pickVersionDir is null)
            throw new InvalidOperationException($"No Microsoft.NETCore.App.Host pack versions found under {packsRoot}.");

        var appHostTemplate = Path.Combine(pickVersionDir, "runtimes", rid, "native", GetAppHostFileName());
        if (!File.Exists(appHostTemplate))
            throw new FileNotFoundException($"AppHost template not found at '{appHostTemplate}'.", appHostTemplate);

        // 5) Destination apphost (next to your produced .dll)
        var exeName = GetExeFileName(Path.GetFileNameWithoutExtension(outputPath));
        var appHostDestination = Path.Combine(Path.GetDirectoryName(outputPath)!, exeName);

        // 6) Call HostModel with the full overload
        HostWriter.CreateAppHost(
            appHostTemplate,
            appHostDestination,
            Path.GetFileName(outputPath),
            windowsGraphicalUserInterface: false,
            assemblyToCopyResorcesFrom: null
        );

        // 7) Finalize the host on Unix/macOS (order matters)
        TryMakeExecutable(appHostDestination);      // chmod +x
        TryRemoveQuarantine(appHostDestination);    // remove com.apple.quarantine if present (macOS only)
        TryAdHocSign(appHostDestination);           // codesign -s - (macOS only)

        // 8) Emit runtimeconfig.json using the picked pack version (product version)
        var runtimeConfigPath = Path.ChangeExtension(outputPath, ".runtimeconfig.json");
        var tfm = TargetFrameworkMoniker.Parse(targetFramework);
        var productVersion = Path.GetFileName(pickVersionDir); // e.g. "10.0.0" or "10.0.1"
        File.WriteAllText(runtimeConfigPath, $@"{{
  ""runtimeOptions"": {{
    ""tfm"": ""{tfm}"",
    ""framework"": {{
      ""name"": ""Microsoft.NETCore.App"",
      ""version"": ""{productVersion}""
    }}
  }}
}}");
    }

    private static string? GetDotnetRoot()
    {
        // Respect env vars first
        var root = Environment.GetEnvironmentVariable("DOTNET_ROOT");
        if (!string.IsNullOrEmpty(root) && Directory.Exists(root))
            return root;

        // Fallbacks by OS (common installs). Allow override via RAVEN_DOTNET_ROOT too.
        var overrideRoot = Environment.GetEnvironmentVariable("RAVEN_DOTNET_ROOT");
        if (!string.IsNullOrEmpty(overrideRoot) && Directory.Exists(overrideRoot))
            return overrideRoot;

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            var p1 = @"C:\Program Files\dotnet";
            var p2 = @"C:\Program Files (x86)\dotnet";
            if (Directory.Exists(p1)) return p1;
            if (Directory.Exists(p2)) return p2;
            return null;
        }

        if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            // Homebrew default first, then the official installer
            var p1 = "/usr/local/share/dotnet";
            var p2 = "/usr/share/dotnet";
            var p3 = "/opt/homebrew/opt/dotnet/libexec"; // Apple Silicon brew
            if (Directory.Exists(p3)) return p3;
            if (Directory.Exists(p1)) return p1;
            if (Directory.Exists(p2)) return p2;
            return null;
        }

        // Linux common paths
        var l1 = "/usr/share/dotnet";
        var l2 = "/usr/local/share/dotnet";
        if (Directory.Exists(l1)) return l1;
        if (Directory.Exists(l2)) return l2;

        return null;
    }

    private static string GetDefaultRid()
    {
        var arch = RuntimeInformation.ProcessArchitecture switch
        {
            Architecture.X64 => "x64",
            Architecture.X86 => "x86",
            Architecture.Arm64 => "arm64",
            Architecture.Arm => "arm",
            _ => "x64"
        };

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) return $"win-{arch}";
        if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX)) return $"osx-{arch}";
        // crude default for Linux; adjust if you need musl (alpine)
        return $"linux-{arch}";
    }

    private static string GetAppHostFileName()
        => RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "apphost.exe" : "apphost";

    private static string GetExeFileName(string baseName)
        => RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? $"{baseName}.exe" : baseName;

    private static void TryMakeExecutable(string path)
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) return;
        try
        {
            // chmod +x

            // 0755 in octal
            const int mode = 0x1ED; // decimal 493
            _ = NativeMethods.chmod(path, mode);
        }
        catch
        {
            // Best effort; optionally shell out to "chmod +x"
            try { System.Diagnostics.Process.Start("chmod", $"+x \"{path}\"")?.WaitForExit(); } catch { /* ignore */ }
        }
    }

    static void TryAdHocSign(string path)
    {
        if (!RuntimeInformation.IsOSPlatform(OSPlatform.OSX)) return;
        try
        {
            var psi = new System.Diagnostics.ProcessStartInfo(
                "codesign", $"-s - --force --timestamp=none \"{path}\"")
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };
            using var p = System.Diagnostics.Process.Start(psi);
            p?.WaitForExit();
        }
        catch { /* best effort */ }
    }

    static void TryRemoveQuarantine(string path)
    {
        if (!RuntimeInformation.IsOSPlatform(OSPlatform.OSX)) return;
        try
        {
            var psi = new System.Diagnostics.ProcessStartInfo(
                "xattr", $"-d com.apple.quarantine \"{path}\"")
            {
                RedirectStandardError = true,
                RedirectStandardOutput = true,
                UseShellExecute = false
            };
            using var p = System.Diagnostics.Process.Start(psi);
            p?.WaitForExit();
        }
        catch { /* ignore */ }
    }
}
