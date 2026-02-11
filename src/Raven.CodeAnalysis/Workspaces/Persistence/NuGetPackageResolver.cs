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
        ImmutableArray<ProjectFile.PackageReferenceInfo> packageReferences)
    {
        if (packageReferences.IsDefaultOrEmpty)
            return ImmutableArray<MetadataReference>.Empty;

        var globalPackagesFolder = GetGlobalPackagesFolder();

        if (TryResolveDirectlyFromGlobalCache(globalPackagesFolder, targetFramework, packageReferences, out var directReferences))
            return directReferences.Select(MetadataReference.CreateFromFile).Select(static x => (MetadataReference)x).ToImmutableArray();

        var restoredReferences = RestoreAndResolveReferencesFromAssets(
            projectFilePath,
            globalPackagesFolder,
            targetFramework,
            packageReferences);

        return restoredReferences.Select(MetadataReference.CreateFromFile).Select(static x => (MetadataReference)x).ToImmutableArray();
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

    private static ImmutableArray<string> RestoreAndResolveReferencesFromAssets(
        string projectFilePath,
        string globalPackagesFolder,
        string targetFramework,
        ImmutableArray<ProjectFile.PackageReferenceInfo> packageReferences)
    {
        var projectDirectory = Path.GetDirectoryName(projectFilePath)
            ?? throw new InvalidOperationException($"Unable to determine project directory for '{projectFilePath}'.");
        var scratchDir = Path.Combine(projectDirectory, ".raven", "nuget-restore", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(scratchDir);

        var tempProjectPath = Path.Combine(scratchDir, "Restore.csproj");
        var tempProjectXml = BuildTemporaryRestoreProject(targetFramework, packageReferences);
        File.WriteAllText(tempProjectPath, tempProjectXml);

        try
        {
            Restore(tempProjectPath, projectDirectory, globalPackagesFolder);

            var assetsPath = Path.Combine(scratchDir, "obj", "project.assets.json");
            if (!File.Exists(assetsPath))
                throw new InvalidOperationException($"Restore succeeded but '{assetsPath}' was not generated.");

            return ParseCompileAssets(assetsPath, globalPackagesFolder, targetFramework);
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
        ImmutableArray<ProjectFile.PackageReferenceInfo> packageReferences)
    {
        var sb = new StringBuilder();
        sb.AppendLine("<Project Sdk=\"Microsoft.NET.Sdk\">");
        sb.AppendLine("  <PropertyGroup>");
        sb.AppendLine($"    <TargetFramework>{targetFramework}</TargetFramework>");
        sb.AppendLine("  </PropertyGroup>");
        sb.AppendLine("  <ItemGroup>");
        foreach (var package in packageReferences)
            sb.AppendLine($"    <PackageReference Include=\"{package.Id}\" Version=\"{package.Version}\" />");
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

    private static ImmutableArray<string> ParseCompileAssets(string assetsPath, string globalPackagesFolder, string targetFramework)
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

        var resolved = ImmutableArray.CreateBuilder<string>();
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
                    resolved.Add(fullPath);
            }
        }

        return resolved
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();
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
}
