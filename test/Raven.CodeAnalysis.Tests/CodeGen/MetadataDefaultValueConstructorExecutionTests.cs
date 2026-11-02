using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MetadataDefaultValueConstructorExecutionTests
{
    [Fact]
    public void ImplicitDefaults_ZeroInitializeAndPreserveDeclaredConstructors()
    {
        var tree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*
            func Zero() -> MetadataCtorValue<int> => MetadataCtorValue<int>()
            func Arguments() -> MetadataCtorValue<int> => MetadataCtorValue<int>(42)
            func Initialized() -> MetadataCtorValue<int> => MetadataCtorValue<int>() { Value = 42 }
            func Explicit() -> MetadataExplicitDefaultValue => MetadataExplicitDefaultValue()
            func Optional() -> MetadataOptionalCtorValue => MetadataOptionalCtorValue()
            func Enum() -> MetadataDefaultCtorEnum => MetadataDefaultCtorEnum()
            func Hash() -> int {
                var hash = System.HashCode()
                hash.Add(1)
                return hash.ToHashCode()
            }
            """);
        var compilation = Compilation.Create("MetadataDefaultExecution", [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(MetadataCtorValue<>).Assembly.Location)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var methods = Assembly.Load(image.ToArray()).GetTypes()
            .SelectMany(t => t.GetMethods(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)).ToArray();
        object? Invoke(string name) => Assert.Single(methods.Where(m => m.Name == name)).Invoke(null, null);
        Assert.Equal(new MetadataCtorValue<int>(), Assert.IsType<MetadataCtorValue<int>>(Invoke("Zero")));
        Assert.Equal(new MetadataCtorValue<int>(42), Assert.IsType<MetadataCtorValue<int>>(Invoke("Arguments")));
        Assert.Equal(new MetadataCtorValue<int>(42), Assert.IsType<MetadataCtorValue<int>>(Invoke("Initialized")));
        Assert.Equal(new MetadataExplicitDefaultValue(), Assert.IsType<MetadataExplicitDefaultValue>(Invoke("Explicit")));
        Assert.Equal(new MetadataOptionalCtorValue(), Assert.IsType<MetadataOptionalCtorValue>(Invoke("Optional")));
        Assert.Equal(new MetadataDefaultCtorEnum(), Assert.IsType<MetadataDefaultCtorEnum>(Invoke("Enum")));
        var hash = new HashCode();
        hash.Add(1);
        Assert.Equal(hash.ToHashCode(), Invoke("Hash"));
    }
}

public readonly record struct MetadataCtorValue<T>(T Value);
public readonly record struct MetadataOptionalCtorValue(int Value = 5);
public readonly record struct MetadataExplicitDefaultValue
{
    public int Value { get; }
    public MetadataExplicitDefaultValue() => Value = 42;
}
public enum MetadataDefaultCtorEnum { Nonzero = 5 }
