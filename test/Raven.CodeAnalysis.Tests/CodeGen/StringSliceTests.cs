using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class StringSliceTests
{
    [Theory]
    [InlineData("1..3", "bc")]
    [InlineData("1..=3", "bcd")]
    [InlineData("2..", "cdef")]
    [InlineData("..2", "ab")]
    [InlineData("..", "abcdef")]
    [InlineData("^3..^1", "de")]
    [InlineData("^3..=^1", "def")]
    [InlineData("6..", "")]
    [InlineData("2..2", "")]
    public void DirectAndStoredRanges_ReturnSameSubstring(string range, string expected)
    {
        foreach (var stored in new[] { false, true })
        {
            var source = $$"""
                public class Cases {
                    static func Run() -> string {
                        val text = "abcdef"
                        {{(stored ? $"val range = ({range})" : "")}}
                        return text[{{(stored ? "range" : range)}}]
                    }
                }
                """;
            Assert.Equal(expected, Run(source));
        }
    }

    [Theory]
    [InlineData("3..2")]
    [InlineData("0..7")]
    [InlineData("^7..")]
    public void InvalidBounds_Throw(string range)
    {
        var exception = Assert.Throws<TargetInvocationException>(() => Run($$"""
            public class Cases {
                static func Run() -> string => "abcdef"[{{range}}]
            }
            """));
        Assert.IsType<ArgumentOutOfRangeException>(exception.InnerException);
    }

    [Fact]
    public void ReceiverAndBounds_AreEvaluatedOnceInOrder()
    {
        Assert.Equal("bc:123", Run("""
            public class Cases {
                static var trace: int = 0
                static func Text() -> string {
                    trace = trace * 10 + 1
                    return "abcdef"
                }
                static func Start() -> int {
                    trace = trace * 10 + 2
                    return 1
                }
                static func End() -> int {
                    trace = trace * 10 + 3
                    return 3
                }
                static func Run() -> string {
                    val slice = Text()[Start()..End()]
                    return slice + ":" + trace.ToString()
                }
            }
            """));
    }

    [Fact]
    public void StringSlices_CountUtf16CodeUnits()
    {
        Assert.Equal("😀", Run("""
            public class Cases {
                static func Run() -> string => "a😀b"[1..3]
            }
            """));
    }

    [Fact]
    public void StringSlice_IsNotWritableStorage()
    {
        var compilation = Compilation.Create("StringSliceAssignment",
            [SyntaxTree.ParseText("""
                val text = "abcdef"
                text[1..3] = "xy"
                """)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private static object? Run(string source)
    {
        var compilation = Compilation.Create("StringSlice" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source)], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        return loaded.Assembly.GetType("Cases")!.GetMethod("Run")!.Invoke(null, null);
    }
}
