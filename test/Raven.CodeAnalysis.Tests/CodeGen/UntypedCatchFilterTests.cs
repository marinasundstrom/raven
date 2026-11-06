using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class UntypedCatchFilterTests
{
    [Theory, Trait("Target", "DotNet")]
    [InlineData(true, 1)]
    [InlineData(false, 2)]
    public void DotNet_UntypedCatchEvaluatesGuardBeforeHandlingException(bool enabled, int expected)
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Run(enabled: bool) -> int {
                    try { throw System.Exception() }
                    catch when enabled { return 1 }
                    catch { return 2 }
                }
            }
            """);
        var compilation = Compilation.Create("CatchGuard", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, [enabled]));
    }
}
