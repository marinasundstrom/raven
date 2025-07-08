using System;

namespace Raven.CodeAnalysis;

public readonly struct ProjectId : IEquatable<ProjectId>
{
    public Guid Id { get; }

    public ProjectId(Guid id) => Id = id;

    public static ProjectId CreateNew() => new(Guid.NewGuid());

    public bool Equals(ProjectId other) => Id.Equals(other.Id);
    public override bool Equals(object? obj) => obj is ProjectId other && Equals(other);
    public override int GetHashCode() => Id.GetHashCode();
    public override string ToString() => Id.ToString();
}

public readonly struct DocumentId : IEquatable<DocumentId>
{
    public Guid Id { get; }

    public DocumentId(Guid id) => Id = id;

    public static DocumentId CreateNew() => new(Guid.NewGuid());

    public bool Equals(DocumentId other) => Id.Equals(other.Id);
    public override bool Equals(object? obj) => obj is DocumentId other && Equals(other);
    public override int GetHashCode() => Id.GetHashCode();
    public override string ToString() => Id.ToString();
}
