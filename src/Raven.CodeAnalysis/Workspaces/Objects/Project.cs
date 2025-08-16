using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

public sealed class Project
{
    private readonly ProjectState _state;

    internal Project(ProjectState state) => _state = state;

    internal ProjectState State => _state;

    public ProjectId Id => _state.Info.Id;
    public string Name => _state.Info.Name;
    public VersionStamp Version => _state.Info.Version;

    public IEnumerable<Document> Documents => _state.Documents.Values.Select(s => new Document(s));

    public Document? GetDocument(DocumentId id) =>
        _state.Documents.TryGetValue(id, out var s) ? new Document(s) : null;

    internal Project AddDocument(Document document)
        => new(_state.AddDocument(document.State));

    internal Project UpdateDocument(Document document)
        => new(_state.UpdateDocument(document.State));
}

