using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class TailConditionalCodeGenTests
{
    [Theory]
    [InlineData(true, true, 11)]
    [InlineData(true, false, 22)]
    [InlineData(false, true, 11)]
    [InlineData(false, false, 22)]
    public void TailConditionalInMatchArmPreservesExplicitReturn(bool followingReturn, bool condition, int expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            public class Probe {
                static func Run(condition: bool) -> int {
                    match 1 {
                        1 => {
                            if condition { return 11 } else { return 22 }
                        }
                        _ => { return 33 }
                    }
                    {{(followingReturn ? "return -1" : "")}}
                }
            }
            """);
        var compilation = Compilation.Create("TailConditionalReturn", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, [condition]));
    }

    [Theory]
    [InlineData("if inner", true, true, 11)]
    [InlineData("if inner", true, false, 22)]
    [InlineData("if inner", false, true, 33)]
    [InlineData("if let value: string = candidate", true, true, 11)]
    [InlineData("if let value: string = candidate", true, false, 22)]
    [InlineData("if let value: string = candidate", false, true, 33)]
    public void TailConditionalReturnsSelectedBranch(string header, bool outer, bool inner, int expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            public class Probe {
                static func Run(outer: bool, inner: bool) -> int {
                    let candidate: object = if inner { "yes" } else { 42 }
                    let value = if outer {
                        {{header}} { 11 } else { 22 }
                    } else { 33 }
                    return value
                }
            }
            """);
        var compilation = Compilation.Create("TailConditional", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, [outer, inner]));
    }
}
