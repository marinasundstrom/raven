using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConstantArgumentConversionExecutionTests
{
    [Fact]
    public void ConstantArgumentsPreserveValuesAcrossSourceAndImportedCalls()
    {
        var compilation = Compilation.Create("ConstantArguments", [SyntaxTree.ParseText("""
            import System.Collections.Concurrent.*

            class Probe {
                static func Sum(params values: byte[]) -> int => values[0] + values[1]

                static func Choose(value: int) -> int => 10
                static func Choose(value: byte) -> int => 20

                static func Run() -> int {
                    let values = ConcurrentDictionary<string, byte>()
                    values.TryAdd("key", 255)
                    return values["key"] + Sum(0, 255) + Choose(0)
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(520, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
