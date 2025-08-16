using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

/// <summary>
/// Public facade for a project that delegates to an underlying immutable
/// <see cref="ProjectState"/>.  All mutating operations are performed on the
/// state which returns a new instance to preserve immutability.
/// </summary>
public sealed class Project
{
    private readonly Solution _solution;

    internal ProjectState State { get; }

    internal Project(Solution solution, ProjectState state)
    {
        _solution = solution;
        State = state;
    }

    public ProjectId Id => State.Id;
    public string Name => State.Name;
    public VersionStamp Version => State.Version;
    internal Solution Solution => _solution;

    public IReadOnlyList<ProjectReference> ProjectReferences => State.ProjectReferences;
    public IReadOnlyList<MetadataReference> MetadataReferences => State.MetadataReferences;

    public ImmutableArray<Document> Documents =>
        State.DocumentStates.Ids
            .Select(id => _solution.GetDocument(id))
            .Where(d => d is not null)
            .Cast<Document>()
            .ToImmutableArray();

    public bool ContainsDocument(DocumentId id) =>
        State.DocumentStates.Ids.Contains(id);

    public Document? GetDocument(DocumentId id) =>
        _solution.GetDocument(id);
}
