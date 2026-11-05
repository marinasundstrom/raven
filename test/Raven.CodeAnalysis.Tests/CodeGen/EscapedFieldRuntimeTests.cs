namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class EscapedFieldRuntimeTests
{
    [Theory]
    [InlineData("@field")]
    [InlineData("`field`")]
    public void EscapedFieldReadsLocalInsideAndOutsideAccessor(string spelling)
    {
        var compilation = EscapedFieldKeywordTests.CreateCompilation(spelling, TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(49, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }
}
