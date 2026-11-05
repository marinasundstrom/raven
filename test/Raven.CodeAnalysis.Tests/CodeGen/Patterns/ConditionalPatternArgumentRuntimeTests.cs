namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConditionalPatternArgumentRuntimeTests
{
    [Theory]
    [InlineData(false, "hello", 5)]
    [InlineData(false, null, 0)]
    [InlineData(false, 42, 0)]
    [InlineData(true, "hello", 5)]
    [InlineData(true, null, 0)]
    [InlineData(true, 42, 0)]
    public void NestedCallReadsTheSuccessfulPatternValue(bool bindingPattern, object? value, int expected)
    {
        var compilation = ConditionalPatternArgumentScopeTests.CreateCompilation(bindingPattern, TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Read")!.Invoke(null, [value]));
    }
}
