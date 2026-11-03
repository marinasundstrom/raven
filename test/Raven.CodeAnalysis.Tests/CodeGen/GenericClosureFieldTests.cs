using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class GenericClosureFieldTests
{
    [Theory]
    [InlineData("int", "42")]
    [InlineData("string", "\"value\"")]
    public void GenericMethodSharesCapturedStorageWithCallback(string type, string value)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.*
            public class Probe {
                static func Evaluate<T>(input: T) -> int {
                    var count = 1
                    let read = () => count
                    count = 40
                    return count + read() + 2
                }
                static func Run() -> int => Evaluate<{{type}}>({{value}})
            }
            """);
        var compilation = Compilation.Create("GenericClosureFields", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(82, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void GenericMethodSelectorUsesConstructedLambdaSignature()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            import System.Linq.*
            public open class Node { val Value: int => 42 }
            public class Item : Node {}
            public class Probe {
                static func Values<T>(nodes: IEnumerable<T>) -> int[] where T: Node => nodes.Select(x => x.Value).ToArray()
                static func Run() -> int => Values<Item>([Item()])[0]
            }
            """);
        var compilation = Compilation.Create("GenericSelector", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }


    [Theory]
    [InlineData("int", "42", 42)]
    [InlineData("string", "\"value\"", "value")]
    public void CapturelessGenericSelectorPreservesValueAndReferenceTypes(string type, string value, object expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            import System.Linq.*
            public class Probe {
                static func Copy<T>(values: IEnumerable<T>) -> T[] => values.Select(x => x).ToArray()
                static func Run() -> {{type}} => Copy<{{type}}>([{{value}}])[0]
            }
            """);
        var compilation = Compilation.Create("GenericIdentitySelector", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }


    [Fact]
    public void GenericSelectorInsideGenericTypeKeepsMethodAndTypeParametersDistinct()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Collections.Generic.*
            import System.Linq.*
            public class Mapper<TOuter> {
                static func Copy<T>(values: IEnumerable<T>) -> T[] => values.Select(x => x).ToArray()
            }
            public class Probe {
                static func Run() -> string => Mapper<int>.Copy<string>(["value"])[0]
            }
            """);
        var compilation = Compilation.Create("NestedGenericSelector", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal("value", loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

}
