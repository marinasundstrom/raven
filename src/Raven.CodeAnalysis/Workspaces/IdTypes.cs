using System;

namespace Raven.CodeAnalysis;

public readonly struct SolutionId : IEquatable<SolutionId>
{
    public Guid Id { get; }

    public SolutionId(Guid id) => Id = id;

    public static SolutionId CreateNew() => new(Guid.NewGuid());

    public bool Equals(SolutionId other) => Id.Equals(other.Id);
    public override bool Equals(object? obj) => obj is SolutionId other && Equals(other);
    public override int GetHashCode() => Id.GetHashCode();
    public override string ToString() => Id.ToString();
}

public readonly struct ProjectId : IEquatable<ProjectId>
{
    public SolutionId SolutionId { get; }
    public Guid Id { get; }

    public ProjectId(SolutionId solutionId, Guid id)
    {
        SolutionId = solutionId;
        Id = id;
    }

    public static ProjectId CreateNew(SolutionId solutionId) => new(solutionId, Guid.NewGuid());

    public bool Equals(ProjectId other) => Id.Equals(other.Id) && SolutionId.Equals(other.SolutionId);
    public override bool Equals(object? obj) => obj is ProjectId other && Equals(other);
    public override int GetHashCode() => HashCode.Combine(SolutionId, Id);
    public override string ToString() => $"{SolutionId}/{Id}";
}

public readonly struct DocumentId : IEquatable<DocumentId>
{
    public ProjectId ProjectId { get; }
    public Guid Id { get; }

    public DocumentId(ProjectId projectId, Guid id)
    {
        ProjectId = projectId;
        Id = id;
    }

    public static DocumentId CreateNew(ProjectId projectId) => new(projectId, Guid.NewGuid());

    public bool Equals(DocumentId other) => Id.Equals(other.Id) && ProjectId.Equals(other.ProjectId);
    public override bool Equals(object? obj) => obj is DocumentId other && Equals(other);
    public override int GetHashCode() => HashCode.Combine(ProjectId, Id);
    public override string ToString() => $"{ProjectId}/{Id}";
}
