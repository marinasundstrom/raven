using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableGenericMetadataExecutionTests
{
    [Fact]
    public void InferredNullableValueArgumentPreservesNullAndFallbackAtRuntime()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Coalesce<T>(value: T?, fallback: T) -> T {
                    if let present: T = value { return present }
                    return fallback
                }
                static func Run(value: int?) -> int? => Coalesce(value, 7)
            }
            """);
        var compilation = Compilation.Create("NullableInferenceExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Run")!;
        Assert.Equal(typeof(int?), method.ReturnType);
        Assert.Equal(7, method.Invoke(null, [null]));
        Assert.Equal(3, method.Invoke(null, [3]));
    }

    [Fact]
    public void FirstOrDefault_ReferenceAndValueResultsMatchLinq()
    {
        var tree = SyntaxTree.ParseText("""
import System.*
import System.Collections.Generic.*
import System.Linq.*
record struct Item(Value: int)
class Probe {
    static func Equal() -> bool {
        let item = Item(7)
        let comparable: IEquatable<Item> = item
        return comparable.Equals(item)
    }
    static func Find(values: IEnumerable<string>) -> string? {
        var value = values.FirstOrDefault()
        value = value?.Trim()
        return value
    }
    static func Number(values: IEnumerable<int>) -> int => values.FirstOrDefault()
}
""");
        var compilation = Compilation.Create("NullableGenericExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Probe")!;
        Assert.Equal(true, type.GetMethod("Equal")!.Invoke(null, null));
        foreach (var values in new[] { Array.Empty<string>(), new[] { " raven " } })
            Assert.Equal(values.FirstOrDefault()?.Trim(), type.GetMethod("Find")!.Invoke(null, [values]));
        foreach (var values in new[] { Array.Empty<int>(), new[] { 42 } })
            Assert.Equal(values.FirstOrDefault(), type.GetMethod("Number")!.Invoke(null, [values]));
    }
}
