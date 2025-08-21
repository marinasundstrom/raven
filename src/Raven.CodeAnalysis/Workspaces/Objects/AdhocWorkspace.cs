using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// A simple in-memory workspace used for tests and prototyping.
/// </summary>
public sealed class AdhocWorkspace : Workspace
{
    public AdhocWorkspace()
        : base("Adhoc")
    {
    }

    public AdhocWorkspace(HostServices services)
        : base("Adhoc", services)
    {
    }

    /// <summary>
    /// Opens a single document from <paramref name="filePath"/> into the workspace.
    /// If a single project already exists, the document is added to it; otherwise a new
    /// project is created.
    /// </summary>
    public DocumentId OpenDocument(string filePath, string? projectName = null)
    {
        if (filePath is null) throw new ArgumentNullException(nameof(filePath));

        var text = SourceText.From(File.ReadAllText(filePath));
        var name = Path.GetFileName(filePath);

        var solution = CurrentSolution;
        ProjectId projectId;

        if (projectName is not null)
        {
            var existing = solution.Projects.FirstOrDefault(p => p.Name == projectName);
            if (existing is null)
            {
                projectId = ProjectId.CreateNew(solution.Id);
                solution = solution.AddProject(projectId, projectName);
            }
            else
            {
                projectId = existing.Id;
            }
        }
        else if (solution.Projects.Count() == 1)
        {
            projectId = solution.Projects.First().Id;
        }
        else
        {
            projectName = "Adhoc";
            projectId = ProjectId.CreateNew(solution.Id);
            solution = solution.AddProject(projectId, projectName);
        }

        var docId = DocumentId.CreateNew(projectId);
        solution = solution.AddDocument(docId, name, text, filePath);
        TryApplyChanges(solution);
        return docId;
    }

    /// <summary>
    /// Opens all <c>.rav</c> and <c>.rvn</c> files from <paramref name="folderPath"/>
    /// into a new project.
    /// </summary>
    public ProjectId OpenFolder(string folderPath, string? projectName = null)
    {
        if (folderPath is null) throw new ArgumentNullException(nameof(folderPath));
        if (!Directory.Exists(folderPath))
            throw new DirectoryNotFoundException(folderPath);

        var files = RavenFileExtensions.All.SelectMany(ext => Directory.EnumerateFiles(folderPath, $"*{ext}"));

        projectName ??= Path.GetFileName(folderPath);
        var solution = CurrentSolution;
        var projectId = ProjectId.CreateNew(solution.Id);
        solution = solution.AddProject(projectId, projectName);

        foreach (var file in files)
        {
            var text = SourceText.From(File.ReadAllText(file));
            var docId = DocumentId.CreateNew(projectId);
            solution = solution.AddDocument(docId, Path.GetFileName(file), text, file);
        }

        TryApplyChanges(solution);
        return projectId;
    }
}
