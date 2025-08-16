using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed class SolutionState
{
    internal SolutionInfo Info { get; }
    internal ImmutableDictionary<ProjectId, ProjectState> Projects { get; }
    internal ImmutableDictionary<DocumentId, DocumentState> Documents { get; }
    internal WorkspaceChangeKind ChangeKind { get; }
    internal ProjectId? LastProjectId { get; }
    internal DocumentId? LastDocumentId { get; }

    private SolutionState(
        SolutionInfo info,
        ImmutableDictionary<ProjectId, ProjectState> projects,
        ImmutableDictionary<DocumentId, DocumentState> documents,
        WorkspaceChangeKind changeKind,
        ProjectId? lastProjectId,
        DocumentId? lastDocumentId)
    {
        Info = info;
        Projects = projects;
        Documents = documents;
        ChangeKind = changeKind;
        LastProjectId = lastProjectId;
        LastDocumentId = lastDocumentId;
    }

    internal SolutionState(SolutionInfo info)
        : this(info,
               ImmutableDictionary<ProjectId, ProjectState>.Empty,
               ImmutableDictionary<DocumentId, DocumentState>.Empty,
               WorkspaceChangeKind.SolutionAdded,
               null,
               null)
    { }

    private SolutionState With(
        ImmutableDictionary<ProjectId, ProjectState> projects,
        ImmutableDictionary<DocumentId, DocumentState> documents,
        WorkspaceChangeKind kind,
        ProjectId? projectId,
        DocumentId? documentId)
    {
        var newInfo = new SolutionInfo(
            new SolutionInfo.SolutionAttributes(Info.Id, Info.FilePath, Info.Version.GetNewerVersion()),
            projects.Values.Select(p => p.Info));
        return new SolutionState(newInfo, projects, documents, kind, projectId, documentId);
    }

    internal SolutionState AddProject(ProjectState project)
    {
        var newProjects = Projects.Add(project.Info.Id, project);
        return With(newProjects, Documents, WorkspaceChangeKind.ProjectAdded, project.Info.Id, null);
    }

    internal SolutionState AddDocument(DocumentState document)
    {
        var project = Projects[document.Id.ProjectId];
        var newProject = project.AddDocument(document);
        var newProjects = Projects.SetItem(project.Info.Id, newProject);
        var newDocs = Documents.Add(document.Id, document);
        return With(newProjects, newDocs, WorkspaceChangeKind.DocumentAdded, project.Info.Id, document.Id);
    }

    internal SolutionState UpdateDocument(DocumentState document)
    {
        var project = Projects[document.Id.ProjectId];
        var newProject = project.UpdateDocument(document);
        var newProjects = Projects.SetItem(project.Info.Id, newProject);
        var newDocs = Documents.SetItem(document.Id, document);
        return With(newProjects, newDocs, WorkspaceChangeKind.DocumentChanged, project.Info.Id, document.Id);
    }
}

