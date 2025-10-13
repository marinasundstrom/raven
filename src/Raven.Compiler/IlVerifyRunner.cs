using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text.Json;

using Raven.CodeAnalysis;

using Spectre.Console;

namespace Raven;

public static class IlVerifyRunner
{
    public const string PathEnvironmentVariableName = "RAVEN_ILVERIFY_PATH";
    private const string ToolManifestFileName = "dotnet-tools.json";
    private const string ToolPackageId = "dotnet-ilverify";
    private const string ToolCommandName = "ilverify";

    public static bool Verify(string? executablePath, string assemblyPath, Compilation compilation)
    {
        if (string.IsNullOrWhiteSpace(assemblyPath))
            throw new ArgumentException("Assembly path must be provided.", nameof(assemblyPath));

        if (!TryResolveExecutable(executablePath, out var verifierExecutable))
        {
            var hint = string.IsNullOrWhiteSpace(executablePath)
                ? "Install it via 'dotnet tool install --global ilverify', run 'dotnet tool restore' for a local tool, or set RAVEN_ILVERIFY_PATH."
                : $"The configured path '{executablePath}' could not be resolved.";

            var hintMarkup = hint.Replace("[", "[[").Replace("]", "]]");
            AnsiConsole.MarkupLine($"[red]Unable to locate ilverify executable. {hintMarkup}[/]");
            return false;
        }

        var startInfo = new ProcessStartInfo
        {
            FileName = verifierExecutable.FilePath,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false
        };

        foreach (var prefix in verifierExecutable.PrefixArguments)
            startInfo.ArgumentList.Add(prefix);

        startInfo.ArgumentList.Add(assemblyPath);

        var systemModule = typeof(object).Assembly.Location;
        if (!string.IsNullOrEmpty(systemModule))
        {
            var moduleName = Path.GetFileNameWithoutExtension(systemModule);
            if (!string.IsNullOrEmpty(moduleName))
            {
                startInfo.ArgumentList.Add("--system-module");
                startInfo.ArgumentList.Add(moduleName);
            }
        }

        foreach (var reference in GetReferencePaths(compilation))
        {
            startInfo.ArgumentList.Add("-r");
            startInfo.ArgumentList.Add(reference);
        }

        try
        {
            using var process = Process.Start(startInfo);
            if (process is null)
            {
                AnsiConsole.MarkupLine("[red]Failed to start ilverify process.[/]");
                return false;
            }

            var standardOutput = process.StandardOutput.ReadToEnd();
            var standardError = process.StandardError.ReadToEnd();
            process.WaitForExit();

            if (!string.IsNullOrWhiteSpace(standardOutput))
                Console.WriteLine(standardOutput.TrimEnd());

            if (!string.IsNullOrWhiteSpace(standardError))
                Console.Error.WriteLine(standardError.TrimEnd());

            if (process.ExitCode == 0)
            {
                AnsiConsole.MarkupLine("[green]IL verification succeeded.[/]");
                return true;
            }

            AnsiConsole.MarkupLine($"[red]ilverify exited with code {process.ExitCode}.[/]");
            return false;
        }
        catch (Win32Exception ex)
        {
            var name = Path.GetFileName(verifierExecutable.FilePath);
            var message = ex.Message.Replace("[", "[[").Replace("]", "]]");
            AnsiConsole.MarkupLine($"[red]Failed to launch ilverify executable '{name}': {message}[/]");
            return false;
        }
    }

    public static bool TryResolveExecutable(string? configuredPath, out IlVerifyExecutable executable)
    {
        if (!string.IsNullOrWhiteSpace(configuredPath) && TryResolveCandidate(configuredPath!, out var resolved))
        {
            executable = new IlVerifyExecutable(resolved);
            return true;
        }

        var environmentPath = Environment.GetEnvironmentVariable(PathEnvironmentVariableName);
        if (!string.IsNullOrWhiteSpace(environmentPath) && TryResolveCandidate(environmentPath!, out resolved))
        {
            executable = new IlVerifyExecutable(resolved);
            return true;
        }

        if (TryResolveLocalTool(out executable))
            return true;

        var pathValue = Environment.GetEnvironmentVariable("PATH");
        if (!string.IsNullOrEmpty(pathValue))
        {
            var directories = pathValue.Split(
                Path.PathSeparator,
                StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);

            foreach (var directory in directories)
            {
                if (TryResolveCandidate(Path.Combine(directory, ToolCommandName), out resolved))
                {
                    executable = new IlVerifyExecutable(resolved);
                    return true;
                }
            }
        }

        executable = default;
        return false;
    }

    private static bool TryResolveCandidate(string candidatePath, out string executablePath)
    {
        if (File.Exists(candidatePath))
        {
            executablePath = candidatePath;
            return true;
        }

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            if (TryResolveWithExtension(candidatePath, ".exe", out executablePath))
                return true;

            if (TryResolveWithExtension(candidatePath, ".cmd", out executablePath))
                return true;

            if (TryResolveWithExtension(candidatePath, ".bat", out executablePath))
                return true;
        }

        executablePath = string.Empty;
        return false;
    }

    private static bool TryResolveLocalTool(out IlVerifyExecutable executable)
    {
        var manifestDirectory = FindManifestDirectory();
        if (manifestDirectory is null)
        {
            executable = default;
            return false;
        }

        var manifestPath = Path.Combine(manifestDirectory, ".config", ToolManifestFileName);
        if (!ManifestContainsIlVerify(manifestPath))
        {
            executable = default;
            return false;
        }

        if (!IsLocalToolRestored())
        {
            executable = default;
            return false;
        }

        executable = new IlVerifyExecutable("dotnet", new[] { "tool", "run", ToolCommandName, "--" });
        return true;
    }

    private static string? FindManifestDirectory()
    {
        try
        {
            var directory = Directory.GetCurrentDirectory();
            while (!string.IsNullOrEmpty(directory))
            {
                var manifestPath = Path.Combine(directory, ".config", ToolManifestFileName);
                if (File.Exists(manifestPath))
                    return directory;

                directory = Directory.GetParent(directory)?.FullName;
            }
        }
        catch (IOException)
        {
        }
        catch (UnauthorizedAccessException)
        {
        }

        return null;
    }

    private static bool ManifestContainsIlVerify(string manifestPath)
    {
        try
        {
            using var stream = File.OpenRead(manifestPath);
            using var document = JsonDocument.Parse(stream);

            if (!document.RootElement.TryGetProperty("tools", out var toolsElement))
                return false;

            foreach (var property in toolsElement.EnumerateObject())
            {
                if (string.Equals(property.Name, ToolPackageId, StringComparison.OrdinalIgnoreCase))
                    return true;
            }
        }
        catch (JsonException)
        {
        }
        catch (IOException)
        {
        }

        return false;
    }

    private static bool IsLocalToolRestored()
    {
        var home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
        if (string.IsNullOrEmpty(home))
            return false;

        var cacheDirectory = Path.Combine(home, ".dotnet", "toolResolverCache");
        if (!Directory.Exists(cacheDirectory))
            return false;

        try
        {
            foreach (var file in Directory.EnumerateFiles(cacheDirectory, ToolPackageId, SearchOption.AllDirectories))
            {
                if (!string.IsNullOrEmpty(file))
                    return true;
            }
        }
        catch (IOException)
        {
        }
        catch (UnauthorizedAccessException)
        {
        }

        return false;
    }

    public readonly struct IlVerifyExecutable
    {
        public IlVerifyExecutable(string filePath)
            : this(filePath, Array.Empty<string>())
        {
        }

        public IlVerifyExecutable(string filePath, IReadOnlyList<string> prefixArguments)
        {
            FilePath = filePath;
            PrefixArguments = prefixArguments;
        }

        public string FilePath { get; }

        public IReadOnlyList<string> PrefixArguments { get; }
    }

    private static bool TryResolveWithExtension(string candidatePath, string extension, out string executablePath)
    {
        var extended = candidatePath.EndsWith(extension, StringComparison.OrdinalIgnoreCase)
            ? candidatePath
            : candidatePath + extension;

        if (File.Exists(extended))
        {
            executablePath = extended;
            return true;
        }

        executablePath = string.Empty;
        return false;
    }

    private static IEnumerable<string> GetReferencePaths(Compilation compilation)
    {
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var reference in compilation.References.OfType<PortableExecutableReference>())
        {
            if (string.IsNullOrEmpty(reference.FilePath))
                continue;

            if (seen.Add(reference.FilePath))
                yield return reference.FilePath;
        }

        var runtimeAssembly = typeof(object).Assembly.Location;
        if (!string.IsNullOrEmpty(runtimeAssembly) && seen.Add(runtimeAssembly))
            yield return runtimeAssembly;
    }
}
