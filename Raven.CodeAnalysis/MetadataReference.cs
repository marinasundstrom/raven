namespace Raven.CodeAnalysis;

public abstract class MetadataReference
{
    public static MetadataReference CreateFromFile(string location)
    {
        return new PortableExecutableReference(location);
    }
}

public sealed class CompilationReference : MetadataReference
{
    internal CompilationReference(Compilation compilation)
    {
        Compilation = compilation;
    }

    public Compilation Compilation { get; }
}

public sealed class PortableExecutableReference : MetadataReference
{
    internal PortableExecutableReference(string location)
    {
        Location = location;
    }

    public string Location { get; }
}