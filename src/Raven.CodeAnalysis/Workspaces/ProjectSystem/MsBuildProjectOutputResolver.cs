using System;

namespace Raven.CodeAnalysis;

public static class MsBuildProjectOutputResolver
{
    public static string ResolveProjectOutputPath(
        string projectFilePath,
        string? requestedTargetFramework = null,
        RavenProjectConventions? conventions = null,
        string? requestedConfiguration = null)
    {
        if (string.IsNullOrWhiteSpace(projectFilePath))
            throw new ArgumentException("Project file path is required.", nameof(projectFilePath));

        MsBuildLocatorRegistration.EnsureRegistered();

        var evaluation = MsBuildProjectEvaluator.Evaluate(
            projectFilePath,
            conventions ?? RavenProjectConventions.Default,
            requestedTargetFramework,
            requestedConfiguration);

        return evaluation.OutputPath;
    }

    public static string ResolveProjectOutputDirectory(
        string projectFilePath,
        string? requestedTargetFramework = null,
        RavenProjectConventions? conventions = null,
        string? requestedConfiguration = null)
    {
        if (string.IsNullOrWhiteSpace(projectFilePath))
            throw new ArgumentException("Project file path is required.", nameof(projectFilePath));

        MsBuildLocatorRegistration.EnsureRegistered();

        var evaluation = MsBuildProjectEvaluator.Evaluate(
            projectFilePath,
            conventions ?? RavenProjectConventions.Default,
            requestedTargetFramework,
            requestedConfiguration);

        return evaluation.OutputDirectory;
    }
}
