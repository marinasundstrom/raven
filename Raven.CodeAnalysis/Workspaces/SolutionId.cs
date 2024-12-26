using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis;

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
