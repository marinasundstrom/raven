using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>
/// Lightweight, immutable representation of a loaded solution. Every edit returns a new
/// <see cref="Solution"/> instance that shares as much state as possible with the previous one.
/// </summary>
public sealed class Solution
{
    private readonly ImmutableDictionary<ProjectId, Project> _projects;

    internal SolutionInfo Info => SolutionState.SolutionAttributes;

    internal SolutionCompilationState CompilationState { get; }

    internal SolutionState SolutionState => CompilationState.SolutionState;

    public Solution(SolutionInfo info)
    {
        Info = info;
    }

    public IEnumerable<Project> Projects => _projects.Values;

    public Project? GetProject(ProjectId id)
    {
        if (_projects)
            return _projects.TryGetValue(id, out var p) ? p : null;
    }

    public bool ContainsProject(ProjectId id)
    {
        return SolutionState.ContainsProject(id);
    }

    public Document? GetDocument(DocumentId id) => GetProject(id.ProjectId).GetDocument(id);

    /// <summary>Creates a new project with the given name and empty document list.</summary>
    public Solution AddProject(string name)
    {
        var projectId = ProjectId.CreateNew(Info.Id);
        var project = new ProjectInfo(new ProjectInfo.ProjectAttributes(projectId, name, VersionStamp.Create()),
                                      Enumerable.Empty<DocumentInfo>());
        return AddProject(project);
    }

    public Solution AddProject(ProjectInfo project)
    {
        if (_projects.ContainsKey(project.Id)) return this;

        var newProjects = _projects.Add(project.Id, project);
        var newDocuments = _documents; // none added
        var newInfo = Info.WithProjects(newProjects.Values);
        return new Solution(newInfo, newProjects, newDocuments);
    }

    public Solution AddDocument(ProjectId projectId, string name, Raven.CodeAnalysis.Text.SourceText text)
    {
        if (!_projects.TryGetValue(projectId, out var project)) throw new InvalidOperationException("Project not found");

        var docId = DocumentId.CreateNew(projectId);
        var docInfo = DocumentInfo.Create(docId, name, text);

        var newProject = project.WithDocuments(project.Documents.Concat(new[] { docInfo }));
        var newProjects = _projects.SetItem(projectId, newProject);
        var newDocuments = _documents.Add(docId, docInfo);

        var newInfo = Info.WithProjects(newProjects.Values);
        return new Solution(newInfo, newProjects, newDocuments);
    }

    public VersionStamp Version => Info.Version;

    public override string ToString() => $"Solution {Info.Id} ({_projects.Count} projects)";
}
