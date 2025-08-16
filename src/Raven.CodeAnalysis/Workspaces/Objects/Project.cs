using System.Collections.Immutable;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Represents a project within a <see cref="Solution"/>. Projects are immutable
/// and contain a collection of <see cref="Document"/> instances.
/// </summary>
public sealed class Project
{
    private readonly ImmutableDictionary<DocumentId, Document> _documents;

    internal Project(ProjectId id, string name, ImmutableDictionary<DocumentId, Document> documents, VersionStamp version)
    {
        Id = id;
        Name = name;
        _documents = documents;
        Version = version;
    }

    public ProjectId Id { get; }
    public string Name { get; }
    public VersionStamp Version { get; }

    public IEnumerable<Document> Documents => _documents.Values;

    public bool ContainsDocument(DocumentId id) => _documents.ContainsKey(id);

    public Document? GetDocument(DocumentId id) => _documents.TryGetValue(id, out var d) ? d : null;

    internal Project AddDocument(Document document)
    {
        var newDocs = _documents.Add(document.Id, document);
        return new Project(Id, Name, newDocs, Version.GetNewerVersion());
    }

    internal Project WithDocument(Document document)
    {
        var newDocs = _documents.SetItem(document.Id, document);
        return new Project(Id, Name, newDocs, Version.GetNewerVersion());
    }
}
