using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis;

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
