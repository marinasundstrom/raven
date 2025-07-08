using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Immutable backing structure for a solution. Acts as the "green" node for Solution.
/// </summary>
public sealed class SolutionAttributes : IEquatable<SolutionAttributes>
{
    /// <summary>
    /// A user-friendly or display name for the solution.
    /// </summary>
    public string Name { get; }

    public SolutionAttributes(string name)
    {
        Name = name;
    }

    public SolutionAttributes WithName(string name)
    {
        return name == Name ? this : new SolutionAttributes(name);
    }

    public bool Equals(SolutionAttributes? other)
    {
        return other != null && Name == other.Name;
    }

    public override bool Equals(object? obj) => obj is SolutionAttributes other && Equals(other);

    public override int GetHashCode() => Name.GetHashCode(StringComparison.Ordinal);

    public override string ToString() => Name;
}