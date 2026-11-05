using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ImportedNullableValueExecutionTests
{
    [Theory]
    [InlineData(null, "none", -1)]
    [InlineData(7, "7", 7)]
    public void ImportedNullableFieldsPropertiesAndGenericReturnsExecute(int? value, string expected, int genericExpected)
    {
        var tree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.*
            class Probe {
                static func Run(value: int?) -> string {
                    let fixture = NullableValueFixture()
                    fixture.Value = value
                    fixture.Field = value
                    if fixture.Value != fixture.Field { return "different" }
                    return NullableValueFixture.Echo(fixture.Value)?.ToString() ?? "none"
                }
                static func Generic(value: int?) -> int {
                    let box = NullableValueBox<int>()
                    box.Value = value
                    return box.Value ?? -1
                }
            }
            """);
        var compilation = Compilation.Create("ImportedNullableExecution", [tree], ImportedNullableValueTests.References(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var probe = loaded.Assembly.GetType("Probe")!;
        Assert.Equal(expected, probe.GetMethod("Run")!.Invoke(null, [value]));
        Assert.Equal(genericExpected, probe.GetMethod("Generic")!.Invoke(null, [value]));
    }
}
