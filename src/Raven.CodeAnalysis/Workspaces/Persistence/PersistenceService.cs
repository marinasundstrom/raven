using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides solution and project persistence for workspaces.
/// </summary>
public class PersistenceService
{
    public virtual Solution OpenSolution(string filePath, Workspace workspace)
    {
        if (workspace is RavenWorkspace raven)
            return SolutionFile.Open(filePath, raven);
        throw new NotSupportedException("Solution persistence requires a RavenWorkspace.");
    }

    public virtual void SaveSolution(Solution solution, string filePath)
        => SolutionFile.Save(solution, filePath);

    public virtual ProjectId OpenProject(Workspace workspace, string projectFilePath)
    {
        if (workspace is not RavenWorkspace raven)
            throw new NotSupportedException("Project persistence requires a RavenWorkspace.");
        var projInfo = ProjectFile.Load(projectFilePath);
        var projectId = raven.AddProject(projInfo.Name, projInfo.TargetFramework, projectFilePath);
        var solution = workspace.CurrentSolution;
        foreach (var doc in projInfo.Documents)
        {
            var docId = DocumentId.CreateNew(projectId);
            solution = solution.AddDocument(docId, doc.Name, doc.Text, doc.FilePath);
        }
        workspace.TryApplyChanges(solution);
        return projectId;
    }

    public virtual void SaveProject(Project project, string filePath)
        => ProjectFile.Save(project, filePath);
}

