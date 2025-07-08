using System.Collections.Immutable;

using static Raven.CodeAnalysis.SolutionInfo;

namespace Raven.CodeAnalysis;

sealed class SolutionState
{
    internal SolutionAttributes SolutionAttributes { get; }
    public SolutionId Id => SolutionAttributes.Id;
    public string? FilePath => SolutionAttributes.FilePath;
    public VersionStamp Version => SolutionAttributes.Version;
    public string? WorkspaceKind { get; set; }
    public int WorkspaceVersion { get; set; }

    public IReadOnlyList<ProjectId> ProjectIds { get; set; }
    public IImmutableDictionary<ProjectId, ProjectState> ProjectStates { get; }
    public IReadOnlyList<AnalyzerReference> AnalyzerReferences { get; }

    public SolutionState(string? workspaceKind, int workspaceVersion, SolutionAttributes solutionAttributes, IReadOnlyList<ProjectId> projectIds, IImmutableDictionary<ProjectId, ProjectState> idToProjectState, IReadOnlyList<AnalyzerReference> analyzerReferences)
    {
        WorkspaceKind = workspaceKind;
        WorkspaceVersion = workspaceVersion;
        SolutionAttributes = solutionAttributes;
        ProjectIds = projectIds;
        ProjectStates = idToProjectState;
        AnalyzerReferences = analyzerReferences;
    }

    public ProjectState? GetProjectState(ProjectId projectId)
    {
        if (!ProjectStates.TryGetValue(projectId, out var state))
        {
            return null;
        }
        return state;
    }

    public bool ContainsProject(ProjectId projectId)
    {
        if (projectId != default)
        {
            return ProjectStates.ContainsKey(projectId);
        }
        return false;
    }


    public SolutionState AddProjects(IReadOnlyList<ProjectInfo> projectInfos)
    {
        var newProjectIds = ProjectIds.Concat(projectInfos.Select(pi => pi.Id));
        var newProjectStates = ProjectStates.AddRange(projectInfos.Select(CreateProjectState).ToDictionary(ps => ps.Id, ps => ps));
        return new SolutionState(WorkspaceKind, WorkspaceVersion, SolutionAttributes, [.. newProjectIds], newProjectStates.ToImmutableDictionary(), AnalyzerReferences);
    }

    private ProjectState CreateProjectState(ProjectInfo pi)
    {
        return new ProjectState(pi,
            new TextDocumentStates<DocumentState>(
                pi.Documents.Select(CreateDocumentState).ToImmutableList()));
    }

    private DocumentState CreateDocumentState(DocumentInfo di)
    {
        var textSource = new TextAndVersionSource();
        //new TextAndVersion(di.Text, VersionStamp.Create())
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
