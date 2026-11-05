namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class InterfacePropertyPatternRuntimeTests
{
    [Theory]
    [InlineData(0, "found")]
    [InlineData(1, "missing")]
    [InlineData(2, "missing")]
    public void PropertyPatternTestsActualInterfaceAndNullableMember(int kind, string expected)
    {
        var compilation = InterfaceReferenceConversionTests.CreateCompilation(TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, [kind]));
    }
}
