using System;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis;

/// <summary>
/// A simple workspace that preconfigures projects with .NET framework references
/// similar to Roslyn's MSBuildWorkspace.
/// </summary>
public sealed class RavenWorkspace : Workspace
{
    private readonly string _sdkVersion;
    private readonly string _defaultTargetFramework;
    private readonly MetadataReference[] _frameworkReferences;

    private RavenWorkspace(string sdkVersion, string defaultTargetFramework, MetadataReference[] frameworkReferences)
        : base("Raven", new HostServices(new SyntaxTreeProvider(), new PersistenceService()))
    {
        _sdkVersion = sdkVersion;
        _defaultTargetFramework = defaultTargetFramework;
        _frameworkReferences = frameworkReferences;
    }

    /// <summary>
    /// Creates a new <see cref="RavenWorkspace"/> with framework references for the
    /// specified SDK version and target framework.
    /// </summary>
    public static RavenWorkspace Create(string sdkVersion = "9.0.*", string targetFramework = "net9.0")
    {
        var refs = GetFrameworkReferencesCore(sdkVersion, targetFramework);
        return new RavenWorkspace(sdkVersion, targetFramework, refs);
    }

    internal string DefaultTargetFramework => _defaultTargetFramework;

    internal MetadataReference[] GetFrameworkReferences(string targetFramework)
        => GetFrameworkReferencesCore(_sdkVersion, targetFramework);

    private static MetadataReference[] GetFrameworkReferencesCore(string sdkVersion, string targetFramework)
    {
        var paths = ReferenceAssemblyPaths.GetReferenceAssemblyPaths(sdkVersion, targetFramework);
        if (paths.Length == 0)
        {
            return new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        }

        return paths
            .Where(p => File.Exists(p))
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
    }

    /// <summary>
    /// Adds a new project to the workspace preloaded with framework references.
    /// </summary>
    public ProjectId AddProject(string name, string? targetFramework = null, string? filePath = null, string? assemblyName = null, CompilationOptions? compilationOptions = null)
    {
        var tfm = targetFramework ?? _defaultTargetFramework;
        var options = compilationOptions ?? new CompilationOptions(OutputKind.ConsoleApplication);

        var solution = CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, name, filePath, tfm, assemblyName, options);
        var references = tfm == _defaultTargetFramework ? _frameworkReferences : GetFrameworkReferences(tfm);
        foreach (var reference in references)
            solution = solution.AddMetadataReference(projectId, reference);
        TryApplyChanges(solution);
        return projectId;
    }

}
