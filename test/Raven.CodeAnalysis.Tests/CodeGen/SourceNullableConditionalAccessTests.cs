using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SourceNullableConditionalAccessTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConditionalAccess_SourceEnumResult_PreservesNullAndValue(bool present)
    {
        var tree = SyntaxTree.ParseText("""
            enum Kind { None, Value }
            class Holder {
                val Kind: Kind => Kind.Value
            }
            class Probe {
                static func Read(present: bool) -> bool {
                    let holder: Holder? = if present { Holder() } else { null }
                    let selected = holder?.Kind ?? Kind.None
                    return selected == Kind.Value
                }
            }
            """);
        var compilation = Compilation.Create("ConditionalSourceEnum" + Guid.NewGuid().ToString("N"), [tree],
            TestMetadataReferences.Default, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Read")!;
        Assert.Equal(present, method.Invoke(null, [present]));
    }
}
