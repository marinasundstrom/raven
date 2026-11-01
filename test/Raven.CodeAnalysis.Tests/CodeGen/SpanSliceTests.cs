using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SpanSliceTests
{
    [Theory]
    [InlineData("1..3", "bc")]
    [InlineData("1..^1", "bc")]
    [InlineData("..2", "ab")]
    [InlineData("2..", "cd")]
    [InlineData("..", "abcd")]
    [InlineData("1..=2", "bc")]
    public void ReadOnlySpanRangesUseSpanBounds(string range, string expected)
    {
        Assert.Equal(expected, Run($$"""
            import System.*
            public class Runner {
                static func Run() -> string {
                    let span = MemoryExtensions.AsSpan("abcd")
                    return span[{{range}}].ToString()
                }
            }
            """));
    }

    [Fact]
    public void SpanSliceSharesStorageAndEvaluatesReceiverAndRangeOnce()
    {
        Assert.Equal("42:2", Run("""
            import System.*
            public class Runner {
                private static val items: int[] = [0, 1, 2, 3]
                private static field calls: int = 0
                static func GetSpan() -> Span<int> { calls += 1; return Span<int>(items) }
                static func GetRange() -> Range { calls += 1; return 1..3 }
                static func Run() -> string {
                    let slice = GetSpan()[GetRange()]
                    items[1] = 42
                    return "${slice[0]}:${calls}"
                }
            }
            """));
    }

    [Fact]
    public void InvalidSpanBoundsThrow()
    {
        var error = Assert.Throws<TargetInvocationException>(() => Run("""
            import System.*
            public class Runner {
                static func Run() -> string {
                    let span = MemoryExtensions.AsSpan("abcd")
                    return span[3..2].ToString()
                }
            }
            """));
        Assert.IsType<ArgumentOutOfRangeException>(error.InnerException);
    }

    private static object? Run(string source)
    {
        var compilation = Compilation.Create("SpanSlices", [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        return loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null);
    }
}
