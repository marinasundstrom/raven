namespace Raven.CodeAnalysis;

public readonly struct SolutionId : IEquatable<SolutionId>
{
    private readonly Guid _id;
    private SolutionId(Guid id) => _id = id;
    public static SolutionId CreateNew() => new(Guid.NewGuid());
    public bool Equals(SolutionId other) => _id.Equals(other._id);
    public override bool Equals(object? obj) => obj is SolutionId other && Equals(other);
    public override int GetHashCode() => _id.GetHashCode();
    public override string ToString() => _id.ToString();

    public static bool operator ==(SolutionId left, SolutionId right)
        => left.Equals(right);
    public static bool operator !=(SolutionId left, SolutionId right)
        => !left.Equals(right);
}

public readonly struct ProjectId : IEquatable<ProjectId>
{
    private readonly SolutionId _solutionId;
    private readonly Guid _id;
    private ProjectId(SolutionId solutionId, Guid guid)
    {
        _solutionId = solutionId;
        _id = guid;
    }

    public SolutionId SolutionId => _solutionId;
    public static ProjectId CreateNew(SolutionId solutionId) => new(solutionId, Guid.NewGuid());

    public bool Equals(ProjectId other) => _id.Equals(other._id) && _solutionId.Equals(other._solutionId);
    public override bool Equals(object? obj) => obj is ProjectId other && Equals(other);
    public override int GetHashCode() => HashCode.Combine(_solutionId, _id);
    public override string ToString() => $"{_solutionId}/{_id}";

    public static bool operator ==(ProjectId left, ProjectId right)
        => left.Equals(right);
    public static bool operator !=(ProjectId left, ProjectId right)
        => !left.Equals(right);
}

public readonly struct DocumentId : IEquatable<DocumentId>
{
    private readonly ProjectId _projectId;
    private readonly Guid _id;

    private DocumentId(ProjectId projectId, Guid id)
    {
        _projectId = projectId;
        _id = id;
    }

    public ProjectId ProjectId => _projectId;
    public static DocumentId CreateNew(ProjectId projectId) => new(projectId, Guid.NewGuid());

    public bool Equals(DocumentId other) => _id.Equals(other._id) && _projectId.Equals(other._projectId);
    public override bool Equals(object? obj) => obj is DocumentId other && Equals(other);
    public override int GetHashCode() => HashCode.Combine(_projectId, _id);
    public override string ToString() => $"{_projectId}/{_id}";

    public static bool operator ==(DocumentId left, DocumentId right)
        => left.Equals(right);
    public static bool operator !=(DocumentId left, DocumentId right)
        => !left.Equals(right);
}
