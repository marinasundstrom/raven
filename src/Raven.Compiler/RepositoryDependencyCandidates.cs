namespace Raven;

internal static class RepositoryDependencyCandidates
{
    public static List<string> Create(
        string applicationBaseDirectory,
        string repositoryRoot,
        string projectName,
        string configuration,
        string preferredTargetFramework,
        IEnumerable<string> fallbackTargetFrameworks,
        string assemblyName,
        params string[] applicationRelativeCandidates)
    {
        var candidates = applicationRelativeCandidates
            .Select(path => Path.GetFullPath(Path.Combine(applicationBaseDirectory, path)))
            .ToList();

        AddTargetFrameworkCandidates(candidates, preferredTargetFramework);
        foreach (var targetFramework in fallbackTargetFrameworks)
            AddTargetFrameworkCandidates(candidates, targetFramework);

        return candidates;

        void AddTargetFrameworkCandidates(List<string> destination, string targetFramework)
        {
            var outputDirectory = Path.Combine(
                repositoryRoot,
                "src",
                projectName,
                "bin",
                configuration,
                targetFramework);
            destination.Add(Path.Combine(outputDirectory, assemblyName));
            destination.Add(Path.Combine(outputDirectory, targetFramework, assemblyName));
        }
    }
}
