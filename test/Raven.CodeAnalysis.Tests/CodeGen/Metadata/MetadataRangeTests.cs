using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MetadataRangeTests
{
    [Theory]
    [InlineData("..", 0, 8)]
    [InlineData("2..", 2, 6)]
    [InlineData("..3", 0, 3)]
    [InlineData("2..5", 2, 3)]
    [InlineData("2..=4", 2, 3)]
    [InlineData("..=^2", 0, 7)]
    public void DotNet_RangeWithReferenceAssembliesUsesTargetEndpoints(string expression, int offset, int count)
    {
        var tree = SyntaxTree.ParseText($"class Probe {{ static func Make() -> System.Range => {expression} }}");
        var compilation = Compilation.Create("MetadataRanges", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var range = Assert.IsType<Range>(loaded.Assembly.GetType("Probe", true)!.GetMethod("Make")!.Invoke(null, null));
        Assert.Equal((offset, count), range.GetOffsetAndLength(8));
    }
}
