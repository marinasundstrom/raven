using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public sealed class Solution
{
    private readonly ImmutableDictionary<ProjectId, Project> _projects;

    public SolutionId Id { get; }
    public SolutionAttributes Attributes { get; }
    public VersionStamp Version { get; }

    public ImmutableArray<Project> Projects => _projects.Values.ToImmutableArray();

    public Solution(SolutionId id, string name)
        : this(id, new SolutionAttributes(name))
    {

    }


    public Solution(SolutionId id, SolutionAttributes attributes)
    {
        Id = id;
        Attributes = attributes;
        _projects = ImmutableDictionary<ProjectId, Project>.Empty;
        Version = VersionStamp.Create();
    }

    private Solution(SolutionId id, SolutionAttributes attributes, ImmutableDictionary<ProjectId, Project> projects, VersionStamp version)
    {
        Id = id;
        Attributes = attributes;
        _projects = projects;
        Version = version;
    }

    public Solution WithAttributes(SolutionAttributes newAttributes)
    {
        return newAttributes.Equals(Attributes)
            ? this
            : new Solution(Id, newAttributes, _projects, VersionStamp.Create());
    }

    public Project? GetProject(ProjectId id)
        => _projects.TryGetValue(id, out var project) ? project : null;

    public Solution AddProject(Project project)
    {
        var updated = _projects.SetItem(project.Id, project);
        return new Solution(Id, Attributes, updated, VersionStamp.Create());
    }

    public Solution RemoveProject(ProjectId id)
    {
        var updated = _projects.Remove(id);
        return new Solution(Id, Attributes, updated, VersionStamp.Create());
    }

    public Solution WithProject(Project project)
        => AddProject(project);

    public Solution WithProjectAttributes(ProjectId id, ProjectAttributes newAttributes)
    {
        if (!_projects.TryGetValue(id, out var oldProject))
            return this;

        var newProject = oldProject.WithAttributes(newAttributes);
        return WithProject(newProject);
    }

    public Document? GetDocument(DocumentId id)
    {
        foreach (var project in _projects.Values)
        {
            var doc = project.GetDocument(id);
            if (doc != null)
                return doc;
        }
        return null;
    }

    public Solution WithDocument(Document document)
    {
        var project = GetProject(document.ProjectId);
        if (project == null)
            return this;

        var updatedProject = project.WithDocument(document);
        return WithProject(updatedProject);
    }

    public Solution RemoveDocument(DocumentId id)
    {
        var project = _projects.Values.FirstOrDefault(p => p.ContainsDocument(id));
        if (project == null)
            return this;

        var updatedProject = project.RemoveDocument(id);
        return WithProject(updatedProject);
    }
}