using System;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

public sealed class ProjectAttributes : IEquatable<ProjectAttributes>
{
    public string Name { get; }
    public ImmutableArray<ProjectReference> References { get; }

    public ProjectAttributes(string name, ImmutableArray<ProjectReference> references)
    {
        Name = name;
        References = references;
    }

    public ProjectAttributes WithName(string name)
    {
        return name == Name ? this : new ProjectAttributes(name, References);
    }

    public ProjectAttributes WithReferences(ImmutableArray<ProjectReference> references)
    {
        return references.SequenceEqual(References) ? this : new ProjectAttributes(Name, references);
    }

    public bool Equals(ProjectAttributes? other)
    {
        return other != null &&
               Name == other.Name &&
               References.SequenceEqual(other.References);
    }

    public override bool Equals(object? obj) => Equals(obj as ProjectAttributes);

    public override int GetHashCode() => HashCode.Combine(Name, References);
}
