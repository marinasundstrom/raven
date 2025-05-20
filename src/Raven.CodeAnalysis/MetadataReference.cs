namespace Raven.CodeAnalysis;

public abstract class MetadataReference : IEquatable<MetadataReference>
{
    public static MetadataReference CreateFromFile(string location)
    {
        return new PEReference(location);
    }

    public abstract override bool Equals(object? obj);
    public abstract bool Equals(MetadataReference? other);
    public abstract override int GetHashCode();

    public static bool operator ==(MetadataReference? left, MetadataReference? right)
    {
        if (ReferenceEquals(left, right))
            return true;
        if (left is null || right is null)
            return false;
        return left.Equals(right);
    }

    public static bool operator !=(MetadataReference? left, MetadataReference? right)
    {
        return !(left == right);
    }
}

public sealed class CompilationReference : MetadataReference
{
    public Compilation Compilation { get; }

    internal CompilationReference(Compilation compilation)
    {
        Compilation = compilation;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as MetadataReference);
    }

    public override bool Equals(MetadataReference? other)
    {
        return other is CompilationReference cr &&
               Equals(Compilation, cr.Compilation);
    }

    public override int GetHashCode()
    {
        return Compilation?.GetHashCode() ?? 0;
    }
}

public sealed class PEReference : MetadataReference
{
    public string Location { get; }

    internal PEReference(string location)
    {
        Location = location;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as MetadataReference);
    }

    public override bool Equals(MetadataReference? other)
    {
        return other is PEReference per &&
               string.Equals(Location, per.Location, StringComparison.OrdinalIgnoreCase);
    }

    public override int GetHashCode()
    {
        return StringComparer.OrdinalIgnoreCase.GetHashCode(Location);
    }
}