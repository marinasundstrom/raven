using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class DefaultInterfaceObjectMemberTests
{
    [Theory]
    [InlineData("GetType().Name", "Implementation")]
    [InlineData("self.GetType().Name", "Implementation")]
    [InlineData("ToString()", "Implementation")]
    public void DefaultMember_CanUseObjectMembers(string expression, string expected)
    {
        var source = $$"""
            interface Contract {
                val Name: string? => {{expression}}
            }
            class Implementation : Contract {}
            """;
        var compilation = Compilation.Create("DefaultInterfaceObject", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var instance = Activator.CreateInstance(loaded.Assembly.GetType("Implementation")!);
        Assert.Equal(expected, loaded.Assembly.GetType("Contract")!.GetProperty("Name")!.GetValue(instance));
    }
}
