using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class FieldInitializationTests
{
    [Theory]
    [InlineData("static val", "Holder.Value.Number")]
    [InlineData("static field", "Holder.Value.Number")]
    [InlineData("val", "Holder().Value.Number")]
    [InlineData("field", "Holder().Value.Number")]
    [Trait("Target", "DotNet")]
    public void ObjectInitializersInFieldAndPropertyStorageAreLowered(string declaration, string access)
    {
        var code = $$"""
            class Item {
                var Number: int = 0
            }
            class Holder {
                {{declaration}} Value: Item = Item() { Number = 42 }
            }
            public class Probe {
                static func Run() -> int => {{access}}
            }
            """;
        var compilation = Compilation.Create("StorageObjectInitializer", [SyntaxTree.ParseText(code)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void InstanceAndStaticFieldInitializers_AreEmitted()
    {
        var code = """
class Foo {
    var x: int = 42
    static var y: int = 100
    val X: int {
        get => x
    }
    static val Y: int {
        get => y
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);

        var version = TargetFrameworkResolver.ResolveVersion("net10.0");
        MetadataReference[] references = [
            .. TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(path => MetadataReference.CreateFromFile(path))
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        var diagnostics = string.Join(
            Environment.NewLine,
            result.Diagnostics.Select(static d => d.ToString()));
        Assert.True(result.Success, diagnostics);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var runtimeAssembly = loaded.Assembly;
        var type = runtimeAssembly.GetType("Foo", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var xProp = type.GetProperty("X", BindingFlags.Public | BindingFlags.Instance)!;
        var yProp = type.GetProperty("Y", BindingFlags.Public | BindingFlags.Static)!;

        Assert.Equal(42, (int)xProp.GetValue(instance)!);
        Assert.Equal(100, (int)yProp.GetValue(null)!);
    }
}
