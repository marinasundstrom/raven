using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class AbstractPropertyMetadataTests
{
    [Theory]
    [InlineData("abstract val Value: int { get; }")]
    [InlineData("abstract var Value: int { get; set; }")]
    [InlineData("abstract val Value: int")]
    [InlineData("abstract var Value: int")]
    public void AbstractProperty_EmitsAbstractAccessorsWithoutBackingFields(string declaration)
    {
        var tree = SyntaxTree.ParseText("abstract class Contract { " + declaration + " }");
        var compilation = Compilation.Create("AbstractMetadata" + Guid.NewGuid().ToString("N"), [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Contract")!;
        Assert.True(type.IsAbstract);
        Assert.Empty(type.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly));
        Assert.All(type.GetProperty("Value")!.GetAccessors(true), accessor => Assert.True(accessor.IsAbstract));
    }
}
