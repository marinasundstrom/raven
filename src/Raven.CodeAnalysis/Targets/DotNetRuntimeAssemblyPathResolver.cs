using System;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis.Targets;

// Host execution lookup only. These paths never augment the target's semantic
// reference set. An explicit shared root permits deterministic host-layout tests.
internal static class DotNetRuntimeAssemblyPathResolver
{
    internal static string? FindImplementation(string? metadataAssemblyPath, string? sharedFrameworkRoot = null)
    {
        if (string.IsNullOrEmpty(metadataAssemblyPath))
            return null;

        if (TryMapNuGetReferenceAssemblyToRuntimePath(metadataAssemblyPath, sharedFrameworkRoot) is { } nuGetRuntimePath)
            return nuGetRuntimePath;

        try
        {
            var assemblyFileName = Path.GetFileName(metadataAssemblyPath);
            if (string.IsNullOrEmpty(assemblyFileName))
                return null;

            var cursor = Path.GetDirectoryName(metadataAssemblyPath);
            if (cursor is null)
                return null;

            while (cursor is not null && !string.Equals(Path.GetFileName(cursor), "ref", StringComparison.OrdinalIgnoreCase))
            {
                cursor = Path.GetDirectoryName(cursor);
            }

            if (cursor is null)
                return null;

            var versionDirectory = Path.GetDirectoryName(cursor);
            if (versionDirectory is null)
                return null;

            var version = Path.GetFileName(versionDirectory);
            if (string.IsNullOrEmpty(version))
                return null;

            var packDirectory = Path.GetDirectoryName(versionDirectory);
            if (packDirectory is null)
                return null;

            var packId = Path.GetFileName(packDirectory);
            if (string.IsNullOrEmpty(packId))
                return null;

            var packsRoot = Path.GetDirectoryName(packDirectory);
            if (packsRoot is null || !string.Equals(Path.GetFileName(packsRoot), "packs", StringComparison.OrdinalIgnoreCase))
                return null;

            var dotnetRoot = Path.GetDirectoryName(packsRoot);
            if (string.IsNullOrEmpty(dotnetRoot))
                return null;

            var runtimePackId = packId.EndsWith(".Ref", StringComparison.OrdinalIgnoreCase)
                ? packId[..^4]
                : packId;

            var runtimeDirectory = Path.Combine(dotnetRoot, "shared", runtimePackId, version);
            var candidatePath = Path.Combine(runtimeDirectory, assemblyFileName);

            return File.Exists(candidatePath) ? candidatePath : null;
        }
        catch
        {
            return null;
        }
    }

    private static string? TryMapNuGetReferenceAssemblyToRuntimePath(string metadataAssemblyPath, string? sharedFrameworkRoot)
    {
        try
        {
            var normalized = metadataAssemblyPath.Replace(Path.AltDirectorySeparatorChar, Path.DirectorySeparatorChar);
            var refSegment = $"{Path.DirectorySeparatorChar}ref{Path.DirectorySeparatorChar}";
            var refIndex = normalized.IndexOf(refSegment, StringComparison.OrdinalIgnoreCase);
            if (refIndex < 0)
                return null;

            if (TryMapNuGetSharedFrameworkReferenceAssemblyToRuntimePath(normalized, refIndex, sharedFrameworkRoot) is { } sharedFrameworkRuntimePath)
                return sharedFrameworkRuntimePath;

            var libCandidate = normalized[..refIndex] +
                               $"{Path.DirectorySeparatorChar}lib{Path.DirectorySeparatorChar}" +
                               normalized[(refIndex + refSegment.Length)..];
            if (File.Exists(libCandidate))
                return libCandidate;

            var packageRoot = normalized[..refIndex];
            var libRoot = Path.Combine(packageRoot, "lib");
            if (!Directory.Exists(libRoot))
                return null;

            var assemblyFileName = Path.GetFileName(metadataAssemblyPath);
            if (string.IsNullOrWhiteSpace(assemblyFileName))
                return null;

            return Directory
                .EnumerateFiles(libRoot, assemblyFileName, SearchOption.AllDirectories)
                .OrderByDescending(static path => path, StringComparer.OrdinalIgnoreCase)
                .FirstOrDefault();
        }
        catch
        {
            return null;
        }
    }

    private static string? TryMapNuGetSharedFrameworkReferenceAssemblyToRuntimePath(string normalizedMetadataAssemblyPath, int refIndex, string? sharedFrameworkRoot)
    {
        var packageVersionDirectory = normalizedMetadataAssemblyPath[..refIndex];
        var packageDirectory = Path.GetDirectoryName(packageVersionDirectory);
        if (packageDirectory is null)
            return null;

        var packageId = Path.GetFileName(packageDirectory);
        var sharedFrameworkName = packageId.ToLowerInvariant() switch
        {
            "microsoft.aspnetcore.app.ref" => "Microsoft.AspNetCore.App",
            "microsoft.netcore.app.ref" => "Microsoft.NETCore.App",
            _ => null
        };

        if (sharedFrameworkName is null)
            return null;

        var requestedVersion = Path.GetFileName(packageVersionDirectory);
        var assemblyFileName = Path.GetFileName(normalizedMetadataAssemblyPath);
        if (string.IsNullOrWhiteSpace(requestedVersion) || string.IsNullOrWhiteSpace(assemblyFileName))
            return null;

        var sharedRoot = sharedFrameworkRoot;
        if (sharedRoot is null)
        {
            var runtimeDirectory = Path.GetDirectoryName(typeof(object).Assembly.Location);
            sharedRoot = runtimeDirectory is not null
                ? Directory.GetParent(runtimeDirectory)?.Parent?.FullName
                : null;
        }
        if (string.IsNullOrEmpty(sharedRoot))
            return null;

        var frameworkRoot = Path.Combine(sharedRoot, sharedFrameworkName);
        if (!Directory.Exists(frameworkRoot))
            return null;

        var exact = Path.Combine(frameworkRoot, requestedVersion, assemblyFileName);
        if (File.Exists(exact))
            return exact;

        var requestedMajor = TryGetMajorVersion(requestedVersion);
        var candidate = Directory
            .EnumerateDirectories(frameworkRoot)
            .Select(path => new
            {
                Path = path,
                Version = Path.GetFileName(path),
                Major = TryGetMajorVersion(Path.GetFileName(path)),
                NumericVersion = TryGetNumericVersion(Path.GetFileName(path)),
                IsPrerelease = Path.GetFileName(path).Contains('-', StringComparison.Ordinal)
            })
            .Where(x => requestedMajor is null || x.Major == requestedMajor)
            .OrderBy(x => x.IsPrerelease)
            .ThenByDescending(x => x.NumericVersion)
            .ThenByDescending(x => x.Version, StringComparer.OrdinalIgnoreCase)
            .Select(x => Path.Combine(x.Path, assemblyFileName))
            .FirstOrDefault(File.Exists);

        return candidate;
    }

    private static int? TryGetMajorVersion(string? version)
    {
        if (string.IsNullOrWhiteSpace(version))
            return null;

        var dotIndex = version.IndexOf('.');
        var majorText = dotIndex >= 0 ? version[..dotIndex] : version;
        return int.TryParse(majorText, out var major) ? major : null;
    }

    private static Version? TryGetNumericVersion(string? version)
    {
        if (string.IsNullOrWhiteSpace(version))
            return null;

        var prereleaseIndex = version.IndexOf('-');
        var versionText = prereleaseIndex >= 0 ? version[..prereleaseIndex] : version;
        return Version.TryParse(versionText, out var parsed) ? parsed : null;
    }
}
