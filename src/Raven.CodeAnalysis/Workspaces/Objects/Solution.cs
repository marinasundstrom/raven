using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public sealed class Solution
{
    private readonly SolutionState _state;

    private Solution(SolutionState state) => _state = state;

    public Solution(SolutionInfo info) : this(new SolutionState(info)) { }

    internal WorkspaceChangeKind ChangeKind => _state.ChangeKind;
    internal ProjectId? LastProjectId => _state.LastProjectId;
    internal DocumentId? LastDocumentId => _state.LastDocumentId;

    public IEnumerable<Project> Projects => _state.Projects.Values.Select(s => new Project(s));

    public Project? GetProject(ProjectId id) =>
        _state.Projects.TryGetValue(id, out var p) ? new Project(p) : null;

    public Document? GetDocument(DocumentId id) =>
        _state.Documents.TryGetValue(id, out var d) ? new Document(d) : null;

    public VersionStamp Version => _state.Info.Version;

    private Solution With(SolutionState state) => new(state);

    public Solution AddProject(string name)
    {
        var projectId = ProjectId.CreateNew(_state.Info.Id);
        var projectInfo = new ProjectInfo(new ProjectInfo.ProjectAttributes(projectId, name, VersionStamp.Create()), Enumerable.Empty<DocumentInfo>());
        var projectState = new ProjectState(projectInfo, ImmutableDictionary<DocumentId, DocumentState>.Empty);
        return With(_state.AddProject(projectState));
    }

    public Solution AddDocument(DocumentId documentId, string name, SourceText text)
    {
        var docInfo = DocumentInfo.Create(documentId, name, text);
        var docState = new DocumentState(docInfo, VersionStamp.Create());
        return With(_state.AddDocument(docState));
    }

    public Solution WithDocument(Document document)
        => With(_state.UpdateDocument(document.State));

    public Solution WithDocumentText(DocumentId id, SourceText text)
    {
        var document = GetDocument(id) ?? throw new InvalidOperationException("Document not found");
        return WithDocument(document.WithText(text));
    }

    public Solution WithDocumentName(DocumentId id, string name)
    {
        var document = GetDocument(id) ?? throw new InvalidOperationException("Document not found");
        return WithDocument(document.WithName(name));
    }
}

