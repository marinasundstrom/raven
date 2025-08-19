using System.Collections.Immutable;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Immutable representation of a set of projects and documents.
/// </summary>
public sealed class Solution
{
    private readonly ImmutableDictionary<ProjectId, Project> _projects;

    public Solution()
        : this(SolutionId.CreateNew(), VersionStamp.Create(), ImmutableDictionary<ProjectId, Project>.Empty)
    {
    }

    private Solution(SolutionId id, VersionStamp version, ImmutableDictionary<ProjectId, Project> projects)
    {
        Id = id;
        Version = version;
        _projects = projects;
    }

    public SolutionId Id { get; }
    public VersionStamp Version { get; }

    public IEnumerable<Project> Projects => _projects.Values;

    public Project? GetProject(ProjectId id) => _projects.TryGetValue(id, out var p) ? p : null;

    public Document? GetDocument(DocumentId id) => GetProject(id.ProjectId)?.GetDocument(id);

    /// <summary>Adds a new project with the specified name.</summary>
    public Solution AddProject(string name)
    {
        var projectId = ProjectId.CreateNew(Id);
        return AddProject(projectId, name);
    }

    /// <summary>Adds a new project with the specified id and name.</summary>
    public Solution AddProject(ProjectId id, string name)
    {
        if (_projects.ContainsKey(id)) return this;
        var project = new Project(id, name, ImmutableDictionary<DocumentId, Document>.Empty, VersionStamp.Create());
        var newProjects = _projects.Add(id, project);
        return new Solution(Id, Version.GetNewerVersion(), newProjects);
    }

    /// <summary>Adds a new document to the specified project.</summary>
    public Solution AddDocument(DocumentId id, string name, SourceText text)
    {
        if (!_projects.TryGetValue(id.ProjectId, out var project))
            throw new InvalidOperationException("Project not found");

        var tree = SyntaxTreeProvider.TryParse(name, text);
        var document = new Document(id, name, text, tree, null, VersionStamp.Create());
        var newProject = project.AddDocument(document);
        var newProjects = _projects.SetItem(project.Id, newProject);
        return new Solution(Id, Version.GetNewerVersion(), newProjects);
    }

    /// <summary>Replaces an existing document with a new instance.</summary>
    public Solution WithDocument(Document document)
    {
        if (!_projects.TryGetValue(document.Id.ProjectId, out var project))
            throw new InvalidOperationException("Project not found");

        if (!project.ContainsDocument(document.Id))
            throw new InvalidOperationException("Document not found");

        var newProject = project.WithDocument(document);
        var newProjects = _projects.SetItem(project.Id, newProject);
        return new Solution(Id, Version.GetNewerVersion(), newProjects);
    }
}
