using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;

namespace Raven.CodeAnalysis;

internal static class NuGetPackageResolver
{
    public static ImmutableArray<MetadataReference> ResolveReferences(
        string projectFilePath,
        string targetFramework,
        ImmutableArray<ProjectFile.PackageReferenceInfo> packageReferences,
        ImmutableArray<ProjectFile.FrameworkReferenceInfo> frameworkReferences)
    {
        if (packageReferences.IsDefaultOrEmpty && frameworkReferences.IsDefaultOrEmpty)
            return ImmutableArray<MetadataReference>.Empty;

        var globalPackagesFolder = GetGlobalPackagesFolder();
        var resolvedPaths = ImmutableArray.CreateBuilder<string>();

        if (frameworkReferences.IsDefaultOrEmpty &&
            TryResolveDirectlyFromGlobalCache(globalPackagesFolder, targetFramework, packageReferences, out var directReferences))
        {
            resolvedPaths.AddRange(directReferences);
        }
        else
        {
            var restoreResult = RestoreAndResolveReferencesFromAssets(
                projectFilePath,
                globalPackagesFolder,
                targetFramework,
                packageReferences,
                frameworkReferences);

            resolvedPaths.AddRange(restoreResult.PackageReferencePaths);
            resolvedPaths.AddRange(restoreResult.FrameworkPackReferencePaths);
        }

        return resolvedPaths
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .Select(MetadataReference.CreateFromFile)
            .Select(static x => (MetadataReference)x)
            .ToImmutableArray();
    }

    private static bool TryResolveDirectlyFromGlobalCache(
        string globalPackagesFolder,
        string targetFramework,
        ImmutableArray<ProjectFile.PackageReferenceInfo> packageReferences,
        out ImmutableArray<string> references)
    {
        var resolved = ImmutableArray.CreateBuilder<string>();

        foreach (var package in packageReferences)
        {
            var packageRoot = Path.Combine(
                globalPackagesFolder,
                package.Id.ToLowerInvariant(),
                package.Version.ToLowerInvariant());

            var packageReferencesForTarget = ResolvePackageAssemblies(packageRoot, targetFramework);
            if (packageReferencesForTarget.IsDefaultOrEmpty)
            {
                references = ImmutableArray<string>.Empty;
                return false;
            }

            resolved.AddRange(packageReferencesForTarget);
        }

        references = resolved
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();

        return true;
    }

    private static RestoreResolutionResult RestoreAndResolveReferencesFromAssets(
        string projectFilePath,
        string globalPackagesFolder,
        string targetFramework,
        ImmutableArray<ProjectFile.PackageReferenceInfo> packageReferences,
        ImmutableArray<ProjectFile.FrameworkReferenceInfo> frameworkReferences)
    {
        var projectDirectory = Path.GetDirectoryName(projectFilePath)
            ?? throw new InvalidOperationException($"Unable to determine project directory for '{projectFilePath}'.");
        var scratchDir = Path.Combine(projectDirectory, ".raven", "nuget-restore", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(scratchDir);

        var tempProjectPath = Path.Combine(scratchDir, "Restore.csproj");
        var tempProjectXml = BuildTemporaryRestoreProject(targetFramework, packageReferences, frameworkReferences);
        File.WriteAllText(tempProjectPath, tempProjectXml);

        try
        {
            Restore(tempProjectPath, projectDirectory, globalPackagesFolder);

            var assetsPath = Path.Combine(scratchDir, "obj", "project.assets.json");
            if (!File.Exists(assetsPath))
                throw new InvalidOperationException($"Restore succeeded but '{assetsPath}' was not generated.");

            return ParseResolutionFromAssets(assetsPath, globalPackagesFolder, targetFramework);
        }
        finally
        {
            try
            {
                Directory.Delete(scratchDir, recursive: true);
            }
            catch
            {
                // Best effort cleanup.
            }
        }
    }

    private static string BuildTemporaryRestoreProject(
        string targetFramework,
        ImmutableArray<ProjectFile.PackageReferenceInfo> packageReferences,
        ImmutableArray<ProjectFile.FrameworkReferenceInfo> frameworkReferences)
    {
        var sb = new StringBuilder();
        sb.AppendLine("<Project Sdk=\"Microsoft.NET.Sdk\">");
        sb.AppendLine("  <PropertyGroup>");
        sb.AppendLine($"    <TargetFramework>{targetFramework}</TargetFramework>");
        sb.AppendLine("  </PropertyGroup>");
        sb.AppendLine("  <ItemGroup>");
        foreach (var package in packageReferences)
            sb.AppendLine($"    <PackageReference Include=\"{package.Id}\" Version=\"{package.Version}\" />");
        foreach (var framework in frameworkReferences)
            sb.AppendLine($"    <FrameworkReference Include=\"{framework.Name}\" />");
        sb.AppendLine("  </ItemGroup>");
        sb.AppendLine("</Project>");
        return sb.ToString();
    }

    private static void Restore(string tempProjectPath, string workingDirectory, string globalPackagesFolder)
    {
        var startInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            WorkingDirectory = workingDirectory,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false
        };

        startInfo.ArgumentList.Add("restore");
        startInfo.ArgumentList.Add(tempProjectPath);
        startInfo.ArgumentList.Add("--verbosity");
        startInfo.ArgumentList.Add("quiet");
        startInfo.Environment["NUGET_PACKAGES"] = globalPackagesFolder;

        using var process = Process.Start(startInfo)
            ?? throw new InvalidOperationException("Unable to start 'dotnet restore'.");
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit();

        if (process.ExitCode != 0)
        {
            var output = string.Join(
                Environment.NewLine,
                new[] { stdout, stderr }.Where(static text => !string.IsNullOrWhiteSpace(text)));
            throw new InvalidOperationException($"NuGet restore failed for '{tempProjectPath}'.{Environment.NewLine}{output}");
        }
    }

    private static RestoreResolutionResult ParseResolutionFromAssets(string assetsPath, string globalPackagesFolder, string targetFramework)
    {
        using var stream = File.OpenRead(assetsPath);
        using var document = JsonDocument.Parse(stream);
        var root = document.RootElement;

        if (!root.TryGetProperty("targets", out var targets) || targets.ValueKind != JsonValueKind.Object)
            throw new InvalidDataException($"'{assetsPath}' is missing targets.");
        if (!root.TryGetProperty("libraries", out var libraries) || libraries.ValueKind != JsonValueKind.Object)
            throw new InvalidDataException($"'{assetsPath}' is missing libraries.");

        var targetKey = targets.EnumerateObject()
            .Select(static x => x.Name)
            .FirstOrDefault(name => string.Equals(name, targetFramework, StringComparison.OrdinalIgnoreCase)
                || name.StartsWith(targetFramework + "/", StringComparison.OrdinalIgnoreCase));

        if (targetKey is null)
            throw new InvalidDataException($"Could not find target '{targetFramework}' in '{assetsPath}'.");

        var resolvedPackagePaths = ImmutableArray.CreateBuilder<string>();
        foreach (var libraryTarget in targets.GetProperty(targetKey).EnumerateObject())
        {
            var libraryName = libraryTarget.Name;
            var libraryPayload = libraryTarget.Value;
            if (!libraryPayload.TryGetProperty("compile", out var compileAssets) || compileAssets.ValueKind != JsonValueKind.Object)
                continue;

            if (!libraries.TryGetProperty(libraryName, out var libraryInfo))
                continue;
            if (!libraryInfo.TryGetProperty("path", out var libraryPathElement))
                continue;

            var packagePath = libraryPathElement.GetString();
            if (string.IsNullOrWhiteSpace(packagePath))
                continue;

            foreach (var compileAsset in compileAssets.EnumerateObject())
            {
                if (!compileAsset.Name.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
                    continue;
                var fullPath = Path.Combine(
                    globalPackagesFolder,
                    packagePath.Replace('/', Path.DirectorySeparatorChar),
                    compileAsset.Name.Replace('/', Path.DirectorySeparatorChar));
                if (File.Exists(fullPath))
                    resolvedPackagePaths.Add(fullPath);
            }
        }

        var resolvedFrameworkPackPaths = ResolveFrameworkPackReferencesFromAssets(root, targetFramework);

        return new RestoreResolutionResult(
            resolvedPackagePaths.Distinct(StringComparer.OrdinalIgnoreCase).ToImmutableArray(),
            resolvedFrameworkPackPaths);
    }

    private static ImmutableArray<string> ResolveFrameworkPackReferencesFromAssets(JsonElement root, string targetFramework)
    {
        if (!root.TryGetProperty("project", out var projectElement) || projectElement.ValueKind != JsonValueKind.Object)
            return ImmutableArray<string>.Empty;
        if (!projectElement.TryGetProperty("frameworks", out var frameworksElement) || frameworksElement.ValueKind != JsonValueKind.Object)
            return ImmutableArray<string>.Empty;

        var frameworkKey = frameworksElement.EnumerateObject()
            .Select(static x => x.Name)
            .FirstOrDefault(name => string.Equals(name, targetFramework, StringComparison.OrdinalIgnoreCase)
                || name.StartsWith(targetFramework + "/", StringComparison.OrdinalIgnoreCase));
        if (frameworkKey is null)
            return ImmutableArray<string>.Empty;

        var frameworkNode = frameworksElement.GetProperty(frameworkKey);
        if (!frameworkNode.TryGetProperty("downloadDependencies", out var downloadDependencies) ||
            downloadDependencies.ValueKind != JsonValueKind.Array)
            return ImmutableArray<string>.Empty;

        var resolved = ImmutableArray.CreateBuilder<string>();
        foreach (var dep in downloadDependencies.EnumerateArray())
        {
            var name = dep.TryGetProperty("name", out var nameElement) ? nameElement.GetString() : null;
            var versionRange = dep.TryGetProperty("version", out var versionElement) ? versionElement.GetString() : null;
            if (string.IsNullOrWhiteSpace(name) || string.IsNullOrWhiteSpace(versionRange))
                continue;
            if (!name.EndsWith(".Ref", StringComparison.OrdinalIgnoreCase))
                continue;

            var version = NormalizeNuGetVersionRange(versionRange);
            if (string.IsNullOrWhiteSpace(version))
                continue;
            var sharedFrameworkName = name.EndsWith(".Ref", StringComparison.OrdinalIgnoreCase)
                ? name[..^4]
                : name;
            if (string.Equals(sharedFrameworkName, "Microsoft.NETCore.App", StringComparison.OrdinalIgnoreCase))
                continue;

            foreach (var dotnetRoot in GetDotNetRoots())
            {
                var runtimeRefs = ResolveSharedFrameworkRuntimeAssemblies(dotnetRoot, sharedFrameworkName, version);
                if (!runtimeRefs.IsDefaultOrEmpty)
                {
                    resolved.AddRange(runtimeRefs);
                    break;
                }

                var packRoot = Path.Combine(dotnetRoot, "packs", name);
                if (!Directory.Exists(packRoot))
                    continue;

                var selectedVersion = SelectInstalledPackVersion(packRoot, version);
                if (string.IsNullOrWhiteSpace(selectedVersion))
                    continue;

                var packRefDir = Path.Combine(packRoot, selectedVersion, "ref");
                if (!Directory.Exists(packRefDir))
                    continue;

                var tfmDir = Path.Combine(packRefDir, targetFramework);
                if (!Directory.Exists(tfmDir))
                {
                    tfmDir = Directory.EnumerateDirectories(packRefDir)
                        .FirstOrDefault(path => string.Equals(Path.GetFileName(path), targetFramework, StringComparison.OrdinalIgnoreCase))
                        ?? string.Empty;
                }

                if (!Directory.Exists(tfmDir))
                    continue;

                resolved.AddRange(Directory.EnumerateFiles(tfmDir, "*.dll", SearchOption.TopDirectoryOnly));
                break;
            }
        }

        return resolved
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();
    }

    private static ImmutableArray<string> ResolveSharedFrameworkRuntimeAssemblies(
        string dotnetRoot,
        string sharedFrameworkName,
        string requestedVersion)
    {
        var sharedRoot = Path.Combine(dotnetRoot, "shared", sharedFrameworkName);
        if (!Directory.Exists(sharedRoot))
            return ImmutableArray<string>.Empty;

        var selectedVersion = SelectInstalledPackVersion(sharedRoot, requestedVersion);
        if (string.IsNullOrWhiteSpace(selectedVersion))
            return ImmutableArray<string>.Empty;

        var runtimeDir = Path.Combine(sharedRoot, selectedVersion);
        if (!Directory.Exists(runtimeDir))
            return ImmutableArray<string>.Empty;

        return Directory.EnumerateFiles(runtimeDir, "*.dll", SearchOption.TopDirectoryOnly)
            .ToImmutableArray();
    }

    private static string SelectInstalledPackVersion(string packRoot, string requestedVersion)
    {
        var exact = Path.Combine(packRoot, requestedVersion);
        if (Directory.Exists(exact))
            return requestedVersion;

        var requested = ParseVersionLoose(requestedVersion);
        var available = Directory.EnumerateDirectories(packRoot)
            .Select(Path.GetFileName)
            .Where(static name => !string.IsNullOrWhiteSpace(name))
            .Select(name => new { Name = name!, Version = ParseVersionLoose(name!) })
            .Where(static x => x.Version is not null)
            .Select(static x => (x.Name, Version: x.Version!))
            .ToArray();

        if (available.Length == 0)
            return string.Empty;

        if (requested is null)
            return available.OrderByDescending(static x => x.Version).First().Name;

        var compatible = available
            .Where(x => x.Version.Major == requested.Major && x.Version.Minor == requested.Minor)
            .OrderByDescending(static x => x.Version)
            .FirstOrDefault();
        if (!string.IsNullOrWhiteSpace(compatible.Name))
            return compatible.Name;

        return available.OrderByDescending(static x => x.Version).First().Name;
    }

    private static Version? ParseVersionLoose(string value)
    {
        var core = value.Split('-', StringSplitOptions.RemoveEmptyEntries)[0];
        return Version.TryParse(core, out var parsed) ? parsed : null;
    }

    private static ImmutableArray<string> ResolvePackageAssemblies(string packageRoot, string targetFramework)
    {
        if (!Directory.Exists(packageRoot))
            return ImmutableArray<string>.Empty;

        var fromRef = ResolveAssembliesFromGroupFolder(packageRoot, "ref", targetFramework);
        if (!fromRef.IsDefaultOrEmpty)
            return fromRef;

        return ResolveAssembliesFromGroupFolder(packageRoot, "lib", targetFramework);
    }

    private static ImmutableArray<string> ResolveAssembliesFromGroupFolder(string packageRoot, string groupFolder, string targetFramework)
    {
        var root = Path.Combine(packageRoot, groupFolder);
        if (!Directory.Exists(root))
            return ImmutableArray<string>.Empty;

        var targetDirectory = Path.Combine(root, targetFramework);
        if (!Directory.Exists(targetDirectory))
            return ImmutableArray<string>.Empty;

        return Directory.EnumerateFiles(targetDirectory, "*.dll", SearchOption.TopDirectoryOnly)
            .ToImmutableArray();
    }

    private static string GetGlobalPackagesFolder()
    {
        var env = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
        if (!string.IsNullOrWhiteSpace(env))
            return Path.GetFullPath(env);

        var home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
        if (string.IsNullOrWhiteSpace(home))
            throw new InvalidOperationException("Unable to determine user profile for the NuGet global package cache.");
        return Path.Combine(home, ".nuget", "packages");
    }

    private static string NormalizeNuGetVersionRange(string versionRange)
    {
        var trimmed = versionRange.Trim();
        if (trimmed.Length == 0)
            return string.Empty;
        if (!trimmed.StartsWith("[", StringComparison.Ordinal) && !trimmed.StartsWith("(", StringComparison.Ordinal))
            return trimmed;

        var body = trimmed.Trim('[', ']', '(', ')');
        var first = body.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries).FirstOrDefault();
        return first ?? string.Empty;
    }

    private static IEnumerable<string> GetDotNetRoots()
    {
        var envRoots = new[]
        {
            Environment.GetEnvironmentVariable("DOTNET_ROOT"),
            Environment.GetEnvironmentVariable("DOTNET_ROOT(x86)")
        }.Where(static s => !string.IsNullOrWhiteSpace(s));

        foreach (var root in envRoots)
        {
            if (Directory.Exists(root))
                yield return root!;
        }

        if (OperatingSystem.IsWindows())
        {
            var x64 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "dotnet");
            var x86 = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), "dotnet");
            if (Directory.Exists(x64))
                yield return x64;
            if (Directory.Exists(x86))
                yield return x86;
        }
        else if (OperatingSystem.IsMacOS())
        {
            const string appleDefault = "/usr/local/share/dotnet";
            const string brew = "/opt/homebrew/opt/dotnet/libexec";
            if (Directory.Exists(appleDefault))
                yield return appleDefault;
            if (Directory.Exists(brew))
                yield return brew;
        }
        else
        {
            var candidates = new[]
            {
                "/usr/share/dotnet",
                "/usr/lib/dotnet",
                "/snap/dotnet-sdk/current",
            };

            foreach (var candidate in candidates)
            {
                if (Directory.Exists(candidate))
                    yield return candidate;
            }
        }
    }

    private sealed record RestoreResolutionResult(
        ImmutableArray<string> PackageReferencePaths,
        ImmutableArray<string> FrameworkPackReferencePaths);
}
