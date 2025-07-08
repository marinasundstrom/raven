using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.Tracing;
using System.Linq;

namespace Raven.CodeAnalysis;

public sealed class Project
{
    private readonly Solution _solution;
    private readonly ImmutableDictionary<DocumentId, Document> _documents;

    public ProjectId Id { get; }
    public SolutionId SolutionId => Id.SolutionId;
    public ProjectAttributes Attributes { get; }
    public VersionStamp Version { get; }

    public ImmutableArray<Document> Documents => _documents.Values.ToImmutableArray();
    public string Name => Attributes.Name;
    public ImmutableArray<ProjectReference> ProjectReferences => Attributes.References;

    internal Solution Solution => _solution;
    internal Workspace? Workspace => _solution.Workspace;

    public Project(ProjectId id, string name)
    : this(null!, id, new ProjectAttributes(name, []))
    {

    }

    internal Project(Solution solution, ProjectId id, ProjectAttributes attributes)
    {
        _solution = solution;
        Id = id;
        Attributes = attributes;
        _documents = ImmutableDictionary<DocumentId, Document>.Empty;
        Version = VersionStamp.Create();
    }

    internal Project(Solution solution, ProjectId id, ProjectAttributes attributes, ImmutableDictionary<DocumentId, Document> documents, VersionStamp version)
    {
        _solution = solution;
        Id = id;
        Attributes = attributes;
        _documents = documents;
        Version = version;
    }

    public bool ContainsDocument(DocumentId documentId)
        => _documents.ContainsKey(documentId);

    public Document? GetDocument(DocumentId id)
        => _documents.TryGetValue(id, out var doc) ? doc : null;

    public Project AddDocument(Document document)
    {
        var updatedDocs = _documents.SetItem(document.Id, document);
        return new Project(_solution, Id, Attributes, updatedDocs, VersionStamp.Create());
    }

    public Project RemoveDocument(DocumentId id)
    {
        var updatedDocs = _documents.Remove(id);
        return new Project(_solution, Id, Attributes, updatedDocs, VersionStamp.Create());
    }

    public Project WithDocument(Document document)
    {
        return AddDocument(document);
    }

    public Project WithAttributes(ProjectAttributes newAttributes)
    {
        return newAttributes.Equals(Attributes)
            ? this
            : new Project(_solution, Id, newAttributes, _documents, VersionStamp.Create());
    }

    public Project WithReferences(IEnumerable<ProjectReference> references)
    {
        return WithAttributes(Attributes.WithReferences(references.ToImmutableArray()));
    }

    public static Project Create(Solution solution, ProjectId projectId, string name, string assemblyName, string raven)
    {
        return new Project(solution, projectId, new ProjectAttributes(name, []));
    }
}
