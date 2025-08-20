using System;
using System.Linq;

namespace Raven.CodeAnalysis;

/// <summary>
/// A simple workspace that preconfigures projects with .NET framework references
/// similar to Roslyn's MSBuildWorkspace.
/// </summary>
public sealed class RavenWorkspace : Workspace
{
    private readonly MetadataReference[] _frameworkReferences;

    private RavenWorkspace(MetadataReference[] frameworkReferences)
        : base("Raven")
    {
        _frameworkReferences = frameworkReferences;
    }

    /// <summary>
    /// Creates a new <see cref="RavenWorkspace"/> with framework references for the
    /// specified SDK version and target framework.
    /// </summary>
    public static RavenWorkspace Create(string sdkVersion = "9.0.*", string targetFramework = "net9.0")
    {
        var paths = ReferenceAssemblyPaths.GetReferenceAssemblyPaths(sdkVersion, targetFramework);
        MetadataReference[] refs;
        if (paths.Length == 0)
        {
            // fall back to the current runtime if reference assemblies are missing
            refs = new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
        }
        else
        {
            refs = paths
                .Where(p => System.IO.File.Exists(p))
                .Select(MetadataReference.CreateFromFile)
                .ToArray();
        }
        return new RavenWorkspace(refs);
    }

    /// <summary>
    /// Adds a new project to the workspace preloaded with framework references.
    /// </summary>
    public ProjectId AddProject(string name)
    {
        var solution = CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, name);
        foreach (var reference in _frameworkReferences)
            solution = solution.AddMetadataReference(projectId, reference);
        TryApplyChanges(solution);
        return projectId;
    }
}
