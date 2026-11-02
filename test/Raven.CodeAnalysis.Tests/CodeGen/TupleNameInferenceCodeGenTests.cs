using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class TupleNameInferenceCodeGenTests
{
    [Fact]
    public void InferredNames_SurviveGenericLinqCalls()
    {
        var tree = SyntaxTree.ParseText("""
            import System.Linq.*
            class Probe {
                static func Run() -> int {
                    let items = [2, -1, 3]
                    return items.Select((value, index) => (value, index)).Where(pair => pair.value > 0).Sum(pair => pair.value + pair.index)
                }
            }
            """);
        var compilation = Compilation.Create("TupleNames", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(7, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
