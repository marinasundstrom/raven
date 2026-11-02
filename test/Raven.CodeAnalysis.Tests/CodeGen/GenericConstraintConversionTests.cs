using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class GenericConstraintConversionTests
{
    [Fact]
    public void ClassConstraintUpcasts_PreserveReferenceIdentity()
    {
        var tree = SyntaxTree.ParseText("""
            open class Root {}
            class Node : Root {}
            class Probe {
                static func Up<T>(value: T) -> Root where T: Root => value
                static func Cast<T>(value: T) -> Root where T: Root => (Root)value
                static func Run() -> bool {
                    let node = Node()
                    return System.Object.ReferenceEquals(node, Up(node)) && System.Object.ReferenceEquals(node, Cast(node))
                }
            }
            """);
        var compilation = Compilation.Create("GenericUpcast", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(true, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
