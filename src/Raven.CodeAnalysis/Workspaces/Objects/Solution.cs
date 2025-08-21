using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Immutable representation of a set of projects and documents. Acts as a facade
/// over <see cref="SolutionInfo"/> data and lazily creates <see cref="Project"/>
/// instances on demand.
/// </summary>
public sealed class Solution
{
    private readonly SolutionInfo _info;
    private readonly ImmutableDictionary<ProjectId, ProjectInfo> _projectInfos;
    private ImmutableDictionary<ProjectId, Project> _projectCache;

    public Solution(HostServices services, Workspace? workspace = null)
        : this(new SolutionInfo(new SolutionInfo.SolutionAttributes(SolutionId.CreateNew(), string.Empty, VersionStamp.Create()), ImmutableArray<ProjectInfo>.Empty), services, workspace, ImmutableDictionary<ProjectId, Project>.Empty)
    {
    }

    private Solution(SolutionInfo info, HostServices services, Workspace? workspace, ImmutableDictionary<ProjectId, Project> projectCache)
    {
        _info = info;
        Services = services;
        Workspace = workspace;
        _projectInfos = info.Projects.ToImmutableDictionary(p => p.Id);
        _projectCache = projectCache;
    }

    public SolutionId Id => _info.Id;
    public VersionStamp Version => _info.Version;
    public HostServices Services { get; }
    public Workspace? Workspace { get; }

    public IEnumerable<Project> Projects => _projectInfos.Values.Select(info => GetProject(info.Id)!);

    public Project? GetProject(ProjectId id)
    {
        if (!_projectInfos.TryGetValue(id, out var info))
            return null;
        if (!_projectCache.TryGetValue(id, out var project))
        {
            project = new Project(info, this);
            _projectCache = _projectCache.Add(id, project);
        }
        return project;
    }

    public Document? GetDocument(DocumentId id) => GetProject(id.ProjectId)?.GetDocument(id);

    /// <summary>Adds a new project with the specified name.</summary>
    public Solution AddProject(string name, string? filePath = null, string? targetFramework = null, string? assemblyName = null, CompilationOptions? compilationOptions = null)
    {
        var projectId = ProjectId.CreateNew(Id);
        return AddProject(projectId, name, filePath, targetFramework, assemblyName, compilationOptions);
    }

    /// <summary>Adds a new project with the specified id and name.</summary>
    public Solution AddProject(ProjectId id, string name, string? filePath = null, string? targetFramework = null, string? assemblyName = null, CompilationOptions? compilationOptions = null)
    {
        if (_projectInfos.ContainsKey(id)) return this;
        var projAttr = new ProjectInfo.ProjectAttributes(id, name, VersionStamp.Create());
        var projInfo = new ProjectInfo(projAttr, Array.Empty<DocumentInfo>(), filePath: filePath, analyzerReferences: null, targetFramework: targetFramework, compilationOptions: compilationOptions, assemblyName: assemblyName);
        var newInfos = _projectInfos.Add(id, projInfo);
        var newInfo = _info.WithProjects(newInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Removes the project with the specified identifier.</summary>
    public Solution RemoveProject(ProjectId id)
    {
        if (!_projectInfos.ContainsKey(id))
            return this;
        var newInfos = _projectInfos.Remove(id);
        var newInfo = _info.WithProjects(newInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Adds a new document to the specified project.</summary>
    public Solution AddDocument(DocumentId id, string name, SourceText text, string? filePath = null)
    {
        if (!_projectInfos.TryGetValue(id.ProjectId, out var projInfo))
            throw new InvalidOperationException("Project not found");
        var docInfo = DocumentInfo.Create(id, name, text, filePath);
        projInfo = projInfo.WithDocuments(projInfo.Documents.Add(docInfo)).WithVersion(projInfo.Version.GetNewerVersion());
        var newProjInfos = _projectInfos.SetItem(id.ProjectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Creates a new solution with updated text for the specified document.</summary>
    public Solution WithDocumentText(DocumentId id, SourceText newText)
    {
        if (!_projectInfos.TryGetValue(id.ProjectId, out var projInfo))
            throw new InvalidOperationException("Project not found");
        var docInfo = projInfo.Documents.FirstOrDefault(d => d.Id == id) ?? throw new InvalidOperationException("Document not found");
        var updatedDoc = docInfo.WithText(newText);
        var updatedDocs = projInfo.Documents.Select(d => d.Id == id ? updatedDoc : d).ToImmutableArray();
        projInfo = projInfo.WithDocuments(updatedDocs).WithVersion(projInfo.Version.GetNewerVersion());
        var newProjInfos = _projectInfos.SetItem(id.ProjectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Removes a document from the specified project.</summary>
    public Solution RemoveDocument(DocumentId id)
    {
        if (!_projectInfos.TryGetValue(id.ProjectId, out var projInfo))
            return this;
        var docInfo = projInfo.Documents.FirstOrDefault(d => d.Id == id);
        if (docInfo is null)
            return this;
        var updatedDocs = projInfo.Documents.Remove(docInfo);
        projInfo = projInfo.WithDocuments(updatedDocs).WithVersion(projInfo.Version.GetNewerVersion());
        var newProjInfos = _projectInfos.SetItem(id.ProjectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Replaces an existing document with the given instance.</summary>
    public Solution WithDocument(Document document)
    {
        if (document is null) throw new ArgumentNullException(nameof(document));
        var id = document.Id;
        if (!_projectInfos.TryGetValue(id.ProjectId, out var projInfo))
            throw new InvalidOperationException("Project not found");
        if (projInfo.Documents.All(d => d.Id != id))
            throw new InvalidOperationException("Document not found");
        var info = document.Info;
        var updatedDocs = projInfo.Documents.Select(d => d.Id == id ? info : d).ToImmutableArray();
        projInfo = projInfo.WithDocuments(updatedDocs).WithVersion(projInfo.Version.GetNewerVersion());
        var newProjInfos = _projectInfos.SetItem(id.ProjectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Adds a metadata reference to the specified project.</summary>
    public Solution AddMetadataReference(ProjectId projectId, MetadataReference reference)
    {
        if (!_projectInfos.TryGetValue(projectId, out var projInfo))
            throw new InvalidOperationException("Project not found");

        var updatedRefs = projInfo.MetadataReferences.Add(reference);
        projInfo = projInfo.WithMetadataReferences(updatedRefs).WithVersion(projInfo.Version.GetNewerVersion());
        var newProjInfos = _projectInfos.SetItem(projectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Adds a project reference to the specified project.</summary>
    public Solution AddProjectReference(ProjectId projectId, ProjectReference reference)
    {
        if (!_projectInfos.TryGetValue(projectId, out var projInfo))
            throw new InvalidOperationException("Project not found");
        if (!_projectInfos.ContainsKey(reference.ProjectId))
            throw new InvalidOperationException("Referenced project not found");

        var updatedRefs = projInfo.ProjectReferences.Add(reference);
        projInfo = projInfo.WithProjectReferences(updatedRefs).WithVersion(projInfo.Version.GetNewerVersion());
        var newProjInfos = _projectInfos.SetItem(projectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    public Solution WithCompilationOptions(ProjectId projectId, CompilationOptions? compilationOptions)
    {
        if (!_projectInfos.TryGetValue(projectId, out var projInfo))
            throw new InvalidOperationException("Project not found");

        var attr = projInfo.Attributes with { Version = projInfo.Version.GetNewerVersion() };
        projInfo = new ProjectInfo(attr, projInfo.Documents, projInfo.ProjectReferences, projInfo.MetadataReferences, projInfo.AnalyzerReferences, projInfo.FilePath, projInfo.TargetFramework, compilationOptions, projInfo.AssemblyName);
        var newProjInfos = _projectInfos.SetItem(projectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }

    /// <summary>Adds an analyzer reference to the specified project.</summary>
    public Solution AddAnalyzerReference(ProjectId projectId, AnalyzerReference reference)
    {
        if (!_projectInfos.TryGetValue(projectId, out var projInfo))
            throw new InvalidOperationException("Project not found");

        var updated = projInfo.AnalyzerReferences.Add(reference);
        projInfo = projInfo.WithAnalyzerReferences(updated).WithVersion(projInfo.Version.GetNewerVersion());
        var newProjInfos = _projectInfos.SetItem(projectId, projInfo);
        var newInfo = _info.WithProjects(newProjInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Solution(newInfo, Services, Workspace, ImmutableDictionary<ProjectId, Project>.Empty);
    }
}
