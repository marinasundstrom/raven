using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed class ProjectState
{
    internal ProjectInfo Info { get; }
    internal ImmutableDictionary<DocumentId, DocumentState> Documents { get; }

    internal ProjectState(ProjectInfo info, ImmutableDictionary<DocumentId, DocumentState> documents)
    {
        Info = info;
        Documents = documents;
    }

    internal ProjectState AddDocument(DocumentState document)
    {
        var newDocs = Documents.Add(document.Id, document);
        var newInfo = Info.WithDocuments(newDocs.Values.Select(d => d.Info));
        return new ProjectState(newInfo, newDocs);
    }

    internal ProjectState UpdateDocument(DocumentState document)
    {
        var newDocs = Documents.SetItem(document.Id, document);
        var newInfo = Info.WithDocuments(newDocs.Values.Select(d => d.Info));
        return new ProjectState(newInfo, newDocs);
    }
}

