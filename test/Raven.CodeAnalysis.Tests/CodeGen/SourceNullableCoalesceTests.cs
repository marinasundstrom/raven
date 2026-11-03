using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SourceNullableCoalesceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void SourceStructCoalesce_PreservesValueAndShortCircuit(bool generic, bool present)
    {
        var typeParameters = generic ? "<T>" : "";
        var tree = SyntaxTree.ParseText($$"""
            struct Item{{typeParameters}}
            {
                val Number: int
                init(number: int) { Number = number }
            }
            class Probe
            {
                private static field reads: int = 0
                private static field fallbacks: int = 0
                private static func Read{{typeParameters}}(present: bool) -> Item{{typeParameters}}?
                {
                    reads++
                    return if present { Item{{typeParameters}}(3) } else { null }
                }
                private static func Fallback{{typeParameters}}() -> Item{{typeParameters}}
                {
                    fallbacks++
                    return Item{{typeParameters}}(7)
                }
                static func Run{{typeParameters}}(present: bool) -> int
                {
                    reads = 0
                    fallbacks = 0
                    let selected = Read{{typeParameters}}(present) ?? Fallback{{typeParameters}}()
                    return selected.Number * 100 + reads * 10 + fallbacks
                }
                static func Default{{typeParameters}}(present: bool) -> int
                {
                    let value = Read{{typeParameters}}(present)
                    let selected = value ?? default
                    return selected.Number
                }
            }
            """);
        var compilation = Compilation.Create("SourceCoalesce" + Guid.NewGuid().ToString("N"), [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Probe")!;
        var run = type.GetMethod("Run")!;
        var readDefault = type.GetMethod("Default")!;
        if (generic)
        {
            run = run.MakeGenericMethod(typeof(string));
            readDefault = readDefault.MakeGenericMethod(typeof(string));
        }
        Assert.Equal(present ? 310 : 711, run.Invoke(null, [present]));
        Assert.Equal(present ? 3 : 0, readDefault.Invoke(null, [present]));
    }
}
