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

    [Fact]
    public void NonNullableValue_StillRequiresNullableReceiver()
    {
        var compilation = Compilation.Create("value-access", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText("class Probe { static func Run(value: int) -> string? => value?.ToString() }"))
            .AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0404");
    }
}
