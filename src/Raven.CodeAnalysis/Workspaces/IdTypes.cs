using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis;

public readonly struct DocumentId : IEquatable<DocumentId>
{
    private DocumentId(ProjectId projectId, string id)
    {
        ProjectId = projectId;
        Id = id;
    }

    public ProjectId ProjectId { get; }

    public string Id { get; }

    public static DocumentId CreateNewId(ProjectId projectId, string id) => new DocumentId(projectId, id);

    public static DocumentId CreateNewId(ProjectId projectId) => new DocumentId(projectId, Guid.NewGuid().ToString());

    public bool Equals(DocumentId other)
    {
        return ProjectId == other.ProjectId && Id == other.Id;
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is not DocumentId documentId)
            return false;

        return Equals(documentId);
    }

    public override int GetHashCode()
    {
        return ProjectId.GetHashCode() ^ Id.GetHashCode();
    }

    public static bool operator ==(DocumentId a, DocumentId b)
    {
        return a.Equals(b);
    }

    public static bool operator !=(DocumentId a, DocumentId b)
    {
        return !a.Equals(b);
    }

    public override string ToString()
    {
        return Id;
    }
}

public readonly struct ProjectId : IEquatable<ProjectId>
{
    private ProjectId(string id)
    {
        Id = id;
    }

    public string Id { get; }

    public static ProjectId CreateNewId()
    {
        return new ProjectId(Guid.NewGuid().ToString());
    }

    public bool Equals(ProjectId other)
    {
        return Id == other.Id;
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is not ProjectId projectId)
            return false;

        return Equals(projectId);
    }

    public override int GetHashCode()
    {
        return Id.GetHashCode();
    }

    public static bool operator ==(ProjectId a, ProjectId b)
    {
        return a.Equals(b);
    }

    public static bool operator !=(ProjectId a, ProjectId b)
    {
        return !a.Equals(b);
    }

    public override string ToString()
    {
        return Id;
    }
}

public readonly struct SolutionId : IEquatable<SolutionId>
{
    private SolutionId(string id)
    {
        Id = id;
    }

    public string Id { get; }

    public static SolutionId CreateNewId()
    {
        return new SolutionId(Guid.NewGuid().ToString());
    }

    public bool Equals(SolutionId other)
    {
        return Id == other.Id;
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is not SolutionId solutionId)
            return false;

        return Equals(solutionId);
    }

    public override int GetHashCode()
    {
        return Id.GetHashCode();
    }

    public static bool operator ==(SolutionId a, SolutionId b)
    {
        return a.Equals(b);
    }

    public static bool operator !=(SolutionId a, SolutionId b)
    {
        return !a.Equals(b);
    }

    public override string ToString()
    {
        return Id;
    }
}