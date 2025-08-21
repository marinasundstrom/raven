using System;
using System.IO;

namespace Raven.CodeAnalysis;

/// <summary>
/// Extension methods for saving and opening solutions, projects, and documents.
/// </summary>
public static class PersistenceExtensions
{
    public static void OpenSolution(this Workspace workspace, string filePath)
    {
        var service = workspace.Services.PersistenceService
            ?? throw new NotSupportedException("Persistence service is not available.");
        var solution = service.OpenSolution(filePath, workspace);
        workspace.OpenSolution(solution);
    }

    public static void SaveSolution(this Workspace workspace, string filePath)
    {
        var service = workspace.Services.PersistenceService
            ?? throw new NotSupportedException("Persistence service is not available.");
        service.SaveSolution(workspace.CurrentSolution, filePath);
    }

    public static ProjectId OpenProject(this Workspace workspace, string projectFilePath)
    {
        var service = workspace.Services.PersistenceService
            ?? throw new NotSupportedException("Persistence service is not available.");
        return service.OpenProject(workspace, projectFilePath);
    }

    public static void SaveProject(this Workspace workspace, ProjectId projectId, string filePath)
    {
        var service = workspace.Services.PersistenceService
            ?? throw new NotSupportedException("Persistence service is not available.");
        var project = workspace.CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));
        service.SaveProject(project, filePath);
    }

    public static void SaveDocument(this Document document, string filePath)
    {
        if (document is null) throw new ArgumentNullException(nameof(document));
        if (filePath is null) throw new ArgumentNullException(nameof(filePath));
        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);
        File.WriteAllText(filePath, document.Text.ToString());
    }

    public static IDisposable EnableFileWatching(this Workspace workspace)
    {
        var service = workspace.Services.PersistenceService
            ?? throw new NotSupportedException("Persistence service is not available.");
        if (workspace is not RavenWorkspace raven)
            throw new NotSupportedException("File watching requires a RavenWorkspace.");
        return service.EnableFileWatching(raven);
    }
}

