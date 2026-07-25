using System.Reflection;

namespace Raven.CodeAnalysis;

/// <summary>Represents a reference to one or more source generators.</summary>
public sealed class GeneratorReference
{
    private readonly Func<IEnumerable<ISourceGenerator>> _generatorFactory;

    /// <summary>Create a reference from a specific source generator instance.</summary>
    public GeneratorReference(ISourceGenerator generator)
        : this(() => [generator])
    {
    }

    /// <summary>Create a reference from a source generator type.</summary>
    public GeneratorReference(Type generatorType)
        : this(() => [(ISourceGenerator)Activator.CreateInstance(generatorType)!])
    {
        if (!typeof(ISourceGenerator).IsAssignableFrom(generatorType))
            throw new ArgumentException("Type must implement ISourceGenerator.", nameof(generatorType));
    }

    /// <summary>Create a reference from an assembly containing source generators.</summary>
    public GeneratorReference(Assembly assembly)
        : this(CreateAssemblyGeneratorFactory(assembly))
    {
    }

    private GeneratorReference(Func<IEnumerable<ISourceGenerator>> generatorFactory)
    {
        _generatorFactory = generatorFactory ?? throw new ArgumentNullException(nameof(generatorFactory));
    }

    internal IEnumerable<ISourceGenerator> GetGenerators() => _generatorFactory();

    private static Func<IEnumerable<ISourceGenerator>> CreateAssemblyGeneratorFactory(Assembly assembly)
    {
        ArgumentNullException.ThrowIfNull(assembly);

        var generatorTypes = new Lazy<Type[]>(
            () => assembly.GetTypes()
                .Where(t => typeof(ISourceGenerator).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) is not null)
                .OrderBy(static t => t.FullName, StringComparer.Ordinal)
                .ToArray(),
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => generatorTypes.Value.Select(t => (ISourceGenerator)Activator.CreateInstance(t)!);
    }
}
