using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class EnumTypeNameCollisionExecutionTests
{
    [Fact]
    public void EnumConstant_DoesNotEvaluateSameNamedProperty_AndRemainsOptionalDefault()
    {
        var tree = SyntaxTree.ParseText("""
            enum Format { Xml, Markdown }
            class Probe {
                var Reads: int
                val Format: Format {
                    get { Reads++; return .Xml }
                }
                func Pick() -> Format => Format.Markdown
                func WithDefault(value: Format = Format.Markdown) -> Format => value
            }
            """);
        var compilation = Compilation.Create("EnumCollisionExecution", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        var probe = assembly.GetType("Probe")!;
        var instance = Activator.CreateInstance(probe);
        var expected = Enum.Parse(assembly.GetType("Format")!, "Markdown");
        Assert.Equal(expected, probe.GetMethod("Pick")!.Invoke(instance, null));
        Assert.Equal(expected, probe.GetMethod("WithDefault")!.Invoke(instance, [Type.Missing]));
        Assert.Equal(0, probe.GetProperty("Reads")!.GetValue(instance));
    }
}
