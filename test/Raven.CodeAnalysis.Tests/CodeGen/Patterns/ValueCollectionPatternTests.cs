using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ValueCollectionPatternTests
{
    [Theory]
    [InlineData("let [{ Value: string item }] = values else { return \"none\" }; return item", true)]
    [InlineData("let [{ Value: string item }] = values else { return \"none\" }; return item", false)]
    [InlineData("if let [{ Value: string item }] = values { return item }; return \"none\"", true)]
    [InlineData("if let [{ Value: string item }] = values { return item }; return \"none\"", false)]
    [InlineData("if let [{ Value: string item }] whole = values { return whole[0].Value as string ?? \"none\" }; return \"none\"", true)]
    [InlineData("if let [{ Value: string item }] whole = values { return whole[0].Value as string ?? \"none\" }; return \"none\"", false)]
    public void ImmutableValueCollectionPatternsPreserveElementsAndWholeValue(string body, bool populated)
        => AssertRun(body, populated);

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void IteratorMatchesImmutableValueCollection(bool populated)
        => AssertRun("return Read(values).FirstOrDefault() ?? \"none\"", populated, """
            static func Read(values: ImmutableArray<Item>) -> IEnumerable<string> {
                if let [{ Value: string item }] = values {
                    yield item
                }
            }
            """);

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void StructWithDirectCountAndIndexerSupportsPatternCapture(bool whole)
        => AssertRun(whole
            ? "let [{ Value: string item }] captured = values else { return \"none\" }; return captured[0].Value as string ?? \"none\""
            : "let [{ Value: string item }] = values else { return \"none\" }; return item",
            true, """
            public struct Values {
                val Count: int => 1
                val self[index: int]: Item => Item("yes")
            }
            """, "Values()");

    private static void AssertRun(string body, bool populated, string extra = "", string? valueExpression = null)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Immutable.*
            import System.Collections.Generic.*
            import System.Linq.*
            public record struct Item(val Value: object?)
            public class Probe {
                static func Run() -> string {
                    let values = {{valueExpression ?? (populated ? "ImmutableArray.Create<Item>(Item(\"yes\"))" : "ImmutableArray<Item>.Empty")}}
                    {{body}}
                }
                {{extra}}
            }
            """);
        var compilation = Compilation.Create("ValueCollectionPattern", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(populated ? "yes" : "none", loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
