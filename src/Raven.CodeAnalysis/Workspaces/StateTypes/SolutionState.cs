using System.Collections.Immutable;
using System.Linq;

using static Raven.CodeAnalysis.SolutionInfo;

namespace Raven.CodeAnalysis;

/// <summary>
/// Immutable state representation of a <see cref="Solution"/>.  It tracks all
/// project states and remembers the kind of change that produced the state so
/// callers can react appropriately.
/// </summary>
sealed class SolutionState
{
    internal SolutionAttributes SolutionAttributes { get; }

    public SolutionId Id => SolutionAttributes.Id;
    public string FilePath => SolutionAttributes.FilePath;
    public VersionStamp Version => SolutionAttributes.Version;

    public WorkspaceChangeKind ChangeKind { get; }
    public ProjectId? ProjectId { get; }
    public DocumentId? DocumentId { get; }

    public IReadOnlyList<ProjectId> ProjectIds { get; }
    public IImmutableDictionary<ProjectId, ProjectState> ProjectStates { get; }

    public SolutionState(SolutionAttributes attributes,
                         IReadOnlyList<ProjectId> projectIds,
                         IImmutableDictionary<ProjectId, ProjectState> projectStates,
                         WorkspaceChangeKind changeKind = WorkspaceChangeKind.SolutionChanged,
                         ProjectId? projectId = null,
                         DocumentId? documentId = null)
    {
        SolutionAttributes = attributes;
        ProjectIds = projectIds;
        ProjectStates = projectStates;
        ChangeKind = changeKind;
        ProjectId = projectId;
        DocumentId = documentId;
    }

    public ProjectState? GetProjectState(ProjectId projectId)
        => ProjectStates.TryGetValue(projectId, out var state) ? state : null;

    public bool ContainsProject(ProjectId projectId)
        => ProjectStates.ContainsKey(projectId);

    public SolutionState AddProject(ProjectInfo projectInfo)
    {
        var projectState = CreateProjectState(projectInfo);
        var ids = ProjectIds.Concat(new[] { projectInfo.Id }).ToList();
        var states = ProjectStates.Add(projectInfo.Id, projectState);
        var newAttributes = SolutionAttributes with { Version = Version.GetNewerVersion() };
        return new SolutionState(newAttributes, ids, states,
                                 WorkspaceChangeKind.ProjectAdded, projectInfo.Id, null);
    }

    public SolutionState RemoveProject(ProjectId projectId)
    {
        var ids = ProjectIds.Where(id => id != projectId).ToList();
        var states = ProjectStates.Remove(projectId);
        var newAttributes = SolutionAttributes with { Version = Version.GetNewerVersion() };
        return new SolutionState(newAttributes, ids, states,
                                 WorkspaceChangeKind.ProjectRemoved, projectId, null);
    }

    public SolutionState AddDocument(ProjectId projectId, DocumentInfo documentInfo)
    {
        var projectState = ProjectStates[projectId];
        var documentState = CreateDocumentState(documentInfo);
        var newDocStates = new TextDocumentStates<DocumentState>(
            projectState.DocumentStates.States.Values.Append(documentState).ToImmutableList());
        var newProjectInfo = projectState.ProjectInfo.WithDocuments(
            projectState.ProjectInfo.Documents.Concat(new[] { documentInfo }));
        var newProjectState = new ProjectState(newProjectInfo, newDocStates);
        var states = ProjectStates.SetItem(projectId, newProjectState);
        var newAttributes = SolutionAttributes with { Version = Version.GetNewerVersion() };
        return new SolutionState(newAttributes, ProjectIds, states,
                                 WorkspaceChangeKind.DocumentAdded, projectId, documentInfo.Id);
    }

    public DocumentState? GetDocumentState(DocumentId id)
    {
        if (ProjectStates.TryGetValue(id.ProjectId, out var project) &&
            project.DocumentStates.States.TryGetValue(id, out var doc))
            return doc;
        return null;
    }

    private static ProjectState CreateProjectState(ProjectInfo pi)
    {
        return new ProjectState(pi, CreateDocumentStates(pi));
    }

    private static TextDocumentStates<DocumentState> CreateDocumentStates(ProjectInfo pi)
    {
        var docs = pi.Documents.Select(CreateDocumentState).ToImmutableList();
        return new TextDocumentStates<DocumentState>(docs);
    }

    private static DocumentState CreateDocumentState(DocumentInfo di)
    {
        var textSource = new TextAndVersionSource();
        return new DocumentState(di.Attributes, textSource, new ParseOptions());
    }
}
