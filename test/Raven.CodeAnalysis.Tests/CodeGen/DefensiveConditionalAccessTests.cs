using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class DefensiveConditionalAccessTests
{
    [Theory]
    [InlineData("value?.Length", null, -1)]
    [InlineData("value?.Length", "abc", 3)]
    [InlineData("value?.Trim().Length", null, -1)]
    [InlineData("value?.Trim().Length", " abc ", 3)]
    [InlineData("value?[NextIndex()].ToString().Length", null, -1)]
    [InlineData("value?[NextIndex()].ToString().Length", "abc", 101)]
    [InlineData("Once(value)?.Length", null, 99)]
    [InlineData("Once(value)?.Length", "abc", 103)]
    public void NonNullableReference_DefensivelyChecksRuntimeNull(string expression, string? value, int expected)
    {
        var source = $$"""
class Probe {
    private static var calls: int = 0

    private static func NextIndex() -> int {
        calls += 1
        return 0
    }

    private static func Once(value: string) -> string {
        calls += 1
        return value
    }

    static func Run(value: string) -> int {
        let result = {{expression}} ?? -1
        return calls * 100 + result
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("defensive-access", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Run")!;
        Assert.Equal(expected, method.Invoke(null, new object?[] { value }));
    }

    [Theory]
    [InlineData(false, 13)]
    [InlineData(true, 9)]
    public void InheritedPropertyChain_AsConstructorArgument_EvaluatesReceiver(bool missing, int expected)
    {
        var source = """
            namespace Consumer
            import Consumer.Syntax.*
            class Document(val SyntaxTree: SyntaxTree?) {}
            open class Host(val Syntax: Document) {}
            class Pair(val First: int, val Second: int) {
                val Sum: int => First + Second
            }
            class Probe(document: Document) : Host(document) {
                func Run() -> int => Pair(9, Syntax.SyntaxTree?.Text.Length ?? 0).Sum
                static func Test(missing: bool) -> int => Probe(Document(if missing { null } else { SyntaxTree() })).Run()
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("property-chain-access", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source), SyntaxTree.ParseText("namespace Consumer.Syntax\npublic class SyntaxTree { val Text: string => \"abcd\" }"))
            .AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(expected, loaded.Assembly.GetType("Consumer.Probe")!.GetMethod("Test")!.Invoke(null, new object[] { missing }));
    }

    [Fact]
    public void NonNullableValue_StillRequiresNullableReceiver()
    {
        var compilation = Compilation.Create("value-access", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText("class Probe { static func Run(value: int) -> string? => value?.ToString() }"))
            .AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0404");
    }
}
