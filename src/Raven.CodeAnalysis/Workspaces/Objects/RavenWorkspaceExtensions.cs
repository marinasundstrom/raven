using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Extensions for converting workspaces to <see cref="RavenWorkspace"/>.
/// </summary>
public static class RavenWorkspaceExtensions
{
    public static RavenWorkspace ToRavenWorkspace(this AdhocWorkspace workspace, string? sdkVersion = null, string? targetFramework = null)
    {
        if (workspace is null) throw new ArgumentNullException(nameof(workspace));
        var raven = RavenWorkspace.Create(sdkVersion, targetFramework);
        foreach (var project in workspace.CurrentSolution.Projects)
        {
            var projectId = raven.AddProject(project.Name, project.FilePath, project.AssemblyName, project.CompilationOptions);
            var solution = raven.CurrentSolution;
            foreach (var reference in project.MetadataReferences)
                solution = solution.AddMetadataReference(projectId, reference);
            foreach (var doc in project.Documents)
            {
                var docId = DocumentId.CreateNew(projectId);
                solution = solution.AddDocument(docId, doc.Name, doc.Text, doc.FilePath);
            }
            raven.TryApplyChanges(solution);
        }
        return raven;
    }
}

