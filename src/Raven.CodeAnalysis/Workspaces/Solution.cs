using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data.Common;
using System.Linq;

namespace Raven.CodeAnalysis;

public sealed class Solution
{
    private readonly ImmutableDictionary<ProjectId, Project> _projects;
    private readonly Workspace? _workspace;

    public SolutionId Id { get; }
    public SolutionAttributes Attributes { get; }
    public VersionStamp Version { get; }

    public ImmutableArray<Project> Projects => _projects.Values.ToImmutableArray();
    public Workspace? Workspace => _workspace;

    public Solution(SolutionId id, string name, Workspace? workspace = null)
        : this(id, new SolutionAttributes(name), workspace)
    {

    }

    internal Solution(SolutionId id, SolutionAttributes attributes, Workspace? workspace = null)
    {
        Id = id;
        Attributes = attributes;
        _workspace = workspace;
        _projects = ImmutableDictionary<ProjectId, Project>.Empty;
        Version = VersionStamp.Create();
    }

    internal Solution(SolutionId id, SolutionAttributes attributes, ImmutableDictionary<ProjectId, Project> projects, VersionStamp version, Workspace? workspace)
    {
        Id = id;
        Attributes = attributes;
        _projects = projects;
        Version = version;
        _workspace = workspace;
    }

    public Solution WithAttributes(SolutionAttributes newAttributes)
    {
        return newAttributes.Equals(Attributes)
            ? this
            : new Solution(Id, newAttributes, _projects, VersionStamp.Create(), _workspace);
    }

    public Project? GetProject(ProjectId id)
        => _projects.TryGetValue(id, out var project) ? project : null;

    public Solution AddProject(ProjectAttributes attributes)
    {
        var id = ProjectId.CreateNew(Id);
        var project = new Project(this, id, attributes);
        var updated = _projects.SetItem(id, project);
        return new Solution(Id, Attributes, updated, VersionStamp.Create(), _workspace);
    }

    public Solution AddProject(Project project)
    {
        var updated = _projects.SetItem(project.Id, project);
        return new Solution(Id, Attributes, updated, VersionStamp.Create(), _workspace);
    }

    public Solution RemoveProject(ProjectId id)
    {
        var updated = _projects.Remove(id);
        return new Solution(Id, Attributes, updated, VersionStamp.Create(), _workspace);
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
