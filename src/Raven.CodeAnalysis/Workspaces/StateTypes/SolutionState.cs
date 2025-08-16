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

    private DocumentState CreateDocumentState(DocumentInfo di)
    {
        var textAndVersion = new TextAndVersion(di.Text, VersionStamp.Create(), di.FilePath);
        var textSource = new TextAndVersionSource(TextLoader.From(textAndVersion), textAndVersion);
        return new DocumentState(di.Attributes, textSource, new ParseOptions());
    }

    public SolutionState RemoveProjects(IReadOnlyList<ProjectId> projectIds)
    {
        var newProjectIds = ProjectIds.Except(projectIds);
        var newProjectStates = ProjectStates.Where(ps => !projectIds.Contains(ps.Key));
        return new SolutionState(WorkspaceKind, WorkspaceVersion, SolutionAttributes, [.. newProjectIds], newProjectStates.ToImmutableDictionary(), AnalyzerReferences);
    }

    public StateChange AddMetadataReferences(ProjectId projectId, IReadOnlyCollection<MetadataReference> metadataReferences)
    {
        var oldProject = GetProjectState(projectId)!;
        if (metadataReferences.Count == 0)
        {
            return new StateChange(this, oldProject, oldProject);
        }
        return ForkProject(oldProject,
            oldProject.WithMetadataReferences(oldProject.MetadataReferences.Concat(metadataReferences)));
    }

    public StateChange AddProjectReferences(ProjectId projectId, IReadOnlyCollection<ProjectReference> projectReferences)
    {
        var oldProject = GetProjectState(projectId)!;
        if (projectReferences.Count == 0)
        {
            return new StateChange(this, oldProject, oldProject);
        }
        return ForkProject(oldProject,
            oldProject.WithProjectReferences(oldProject.ProjectReferences.Concat(projectReferences)));
    }

    private StateChange ForkProject(ProjectState oldProject, ProjectState newProject)
    {
        // TODO: Handle dependency graph (?)
        return new StateChange(this, oldProject, newProject);
    }

    public SolutionState AddAnalyzerReferences(IReadOnlyCollection<AnalyzerReference> analyzerReferences)
    {
        var newAnalyzerReference = AnalyzerReferences.Concat(analyzerReferences);
        return new SolutionState(WorkspaceKind, WorkspaceVersion, SolutionAttributes, ProjectIds, ProjectStates, newAnalyzerReference.ToList());
    }
}
