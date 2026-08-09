namespace Raven.CommandLine;

internal static class SdkLocator
{
    public static string? TryFindRoot()
    {
        var configuredRoot = Environment.GetEnvironmentVariable("RAVEN_SDK_ROOT");
        if (!string.IsNullOrWhiteSpace(configuredRoot))
        {
            var fullPath = Path.GetFullPath(configuredRoot);
            if (IsSdkRoot(fullPath))
                return fullPath;
        }

        var directory = new DirectoryInfo(AppContext.BaseDirectory);
        while (directory is not null)
        {
            if (IsSdkRoot(directory.FullName))
                return directory.FullName;

            directory = directory.Parent;
        }

        return null;
    }

    public static string? TryFindCompilerDriverPath()
    {
        if (TryFindRoot() is { } sdkRoot)
        {
            var installedCompilerPath = Path.Combine(sdkRoot, "tools", "rvnc", "rvnc.dll");
            if (File.Exists(installedCompilerPath))
                return installedCompilerPath;
        }

        var baseDirectory = new DirectoryInfo(AppContext.BaseDirectory);
        var targetFramework = baseDirectory.Name;
        var configuration = baseDirectory.Parent?.Name;
        if (configuration is not null)
        {
            var developmentCompilerPath = Path.GetFullPath(Path.Combine(
                AppContext.BaseDirectory,
                "..",
                "..",
                "..",
                "..",
                "Raven.Compiler",
                "bin",
                configuration,
                targetFramework,
                "rvnc.dll"));
            if (File.Exists(developmentCompilerPath))
                return developmentCompilerPath;
        }

        var adjacentCompilerPath = Path.Combine(AppContext.BaseDirectory, "rvnc.dll");
        return File.Exists(adjacentCompilerPath) ? adjacentCompilerPath : null;
    }

    private static bool IsSdkRoot(string path)
        => File.Exists(Path.Combine(path, "VERSION")) &&
           File.Exists(Path.Combine(path, "sdk", "build", "Raven.Language.targets"));
}
