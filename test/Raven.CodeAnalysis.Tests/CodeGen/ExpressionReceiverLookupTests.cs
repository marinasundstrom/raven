using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ExpressionReceiverLookupTests
{
    [Fact]
    public void ReturnedValueMemberIsNotReinterpretedAsATypeName()
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ReceiverLookup", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(references).AddSyntaxTrees(SyntaxTree.ParseText("""
                class Value { }
                class Item {
                    var Value: int
                    init(value: int) { Value = value }
                }
                class Probe {
                    static func Make() -> Item { return Item(42) }
                    static func Run() -> int { return Make().Value }
                }
                """));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, []));
    }
}
