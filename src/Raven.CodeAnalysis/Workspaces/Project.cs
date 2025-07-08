using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

public sealed class Project
{
    private readonly ImmutableDictionary<DocumentId, Document> _documents;

    public ProjectId Id { get; }
    public SolutionId SolutionId => Id.SolutionId;
    public ProjectAttributes Attributes { get; }
    public VersionStamp Version { get; }

    public ImmutableArray<Document> Documents => _documents.Values.ToImmutableArray();
    public string Name => Attributes.Name;
    public ImmutableArray<ProjectReference> ProjectReferences => Attributes.References;

    public Project(ProjectId id, ProjectAttributes attributes)
    {
        Id = id;
        Attributes = attributes;
        _documents = ImmutableDictionary<DocumentId, Document>.Empty;
        Version = VersionStamp.Create();
    }

    private Project(ProjectId id, ProjectAttributes attributes, ImmutableDictionary<DocumentId, Document> documents, VersionStamp version)
    {
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
        return new Project(Id, Attributes, updatedDocs, VersionStamp.Create());
    }

    public Project RemoveDocument(DocumentId id)
    {
        var updatedDocs = _documents.Remove(id);
        return new Project(Id, Attributes, updatedDocs, VersionStamp.Create());
    }

    public Project WithDocument(Document document)
    {
        return AddDocument(document);
    }

    public Project WithAttributes(ProjectAttributes newAttributes)
    {
        return newAttributes.Equals(Attributes)
            ? this
            : new Project(Id, newAttributes, _documents, VersionStamp.Create());
    }

    public Project WithReferences(IEnumerable<ProjectReference> references)
    {
        return WithAttributes(Attributes.WithReferences(references.ToImmutableArray()));
    }
}
