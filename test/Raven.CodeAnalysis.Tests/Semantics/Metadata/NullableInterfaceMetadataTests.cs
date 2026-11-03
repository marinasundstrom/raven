using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public static class NullableInterfaceFixtures
{
    public interface IContract<T> { }
    public interface IChild : IContract<string?> { }
    public class Box<T> : IContract<T?> where T : class { }
    public class Derived : Box<string>, IChild { }
    public class Nested : IContract<Dictionary<string, string?[]?>?> { }
    public class Plain : IContract<string> { }
}

[Trait("Target", "DotNet")]
public sealed class NullableInterfaceMetadataTests
{
    private static Compilation Create(string? fixturePath = null)
        => Compilation.Create("NullableInterfaceMetadata", [],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(
                fixturePath ?? typeof(NullableInterfaceFixtures).Assembly.Location)],
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));

    [Fact]
    public void InheritedAndSubstitutedInterfaces_PreserveNullableArguments()
    {
        var compilation = Create();
        var box = compilation.GetTypeByMetadataName(typeof(NullableInterfaceFixtures.Box<>).FullName!)!;
        var constructed = Assert.IsAssignableFrom<NamedTypeSymbol>(box.Construct(compilation.GetSpecialType(SpecialType.System_String)));
        Assert.True(Assert.Single(Assert.Single(constructed.Interfaces).TypeArguments).IsNullable);
        var derived = compilation.GetTypeByMetadataName(typeof(NullableInterfaceFixtures.Derived).FullName!)!;
        var inherited = Assert.Single(derived.AllInterfaces.Where(i => i.Name == "IContract"));
        Assert.True(Assert.Single(inherited.TypeArguments).IsNullable);
        var plain = compilation.GetTypeByMetadataName(typeof(NullableInterfaceFixtures.Plain).FullName!)!;
        Assert.False(Assert.Single(Assert.Single(plain.Interfaces).TypeArguments).IsNullable);
    }

    [Fact]
    public void NestedGenericAndArrayAnnotations_PreserveTheirPositions()
    {
        var compilation = Create();
        var nested = compilation.GetTypeByMetadataName(typeof(NullableInterfaceFixtures.Nested).FullName!)!;
        var argument = Assert.Single(Assert.Single(nested.Interfaces).TypeArguments);
        Assert.True(argument.IsNullable);
        var dictionary = Assert.IsAssignableFrom<NamedTypeSymbol>(argument.GetNonNullableType());
        Assert.False(dictionary.TypeArguments[0].IsNullable);
        Assert.True(dictionary.TypeArguments[1].IsNullable);
        var array = Assert.IsAssignableFrom<ArrayTypeSymbol>(dictionary.TypeArguments[1].GetNonNullableType());
        Assert.True(array.ElementType.IsNullable);
    }

    [Fact]
    public void LoadedInterfaceAnnotations_DoNotReopenReferencePath()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-interface-image-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var path = Path.Combine(directory, "fixture.dll");
            File.Copy(typeof(NullableInterfaceFixtures).Assembly.Location, path);
            var compilation = Create(path);
            var child = compilation.GetTypeByMetadataName(typeof(NullableInterfaceFixtures.IChild).FullName!)!;
            File.Delete(path);
            Assert.True(Assert.Single(Assert.Single(child.Interfaces).TypeArguments).IsNullable);
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }
}
