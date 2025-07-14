using System.Collections.Immutable;

using static Raven.CodeAnalysis.ProjectInfo;

namespace Raven.CodeAnalysis;

public sealed class Project
{
    private readonly Solution _solution;

    private readonly ImmutableDictionary<DocumentId, DocumentInfo> _documents;

    public ProjectId Id => State.Id;
    internal ProjectState State { get; }
    public VersionStamp Version => _solution.Version;
    internal Solution Solution => _solution;

    public string Name => State.Name;

    public IReadOnlyList<ProjectReference> ProjectReferences => State.ProjectReferences;
    public IReadOnlyList<MetadataReference> MetadataReferences => State.MetadataReferences;

    public ImmutableArray<Document> Documents =>
        State.DocumentStates.Ids.Select(id => _solution.GetDocument(id)).Where(d => d is not null).Cast<Document>().ToImmutableArray();

    internal Project(Solution solution, ProjectState state)
    {
        _solution = solution;
        State = state;
    }

    public bool ContainsDocument(DocumentId id)
        => _documentIds.Contains(id);

    public Document? GetDocument(DocumentId id)
        => _documentIds.Contains(id) ? _solution.GetDocument(id) : null;

    public Project WithAttributes(ProjectAttributes newAttributes)
        => _solution.GetProject(Id)!.Solution.WithProjectAttributes(Id, newAttributes).GetProject(Id)!;

    public Project WithReferences(IEnumerable<ProjectReference> references)
        => WithAttributes(Solution.WithProjectMetadataReferences(Id, references.ToImmutableArray()));
}
