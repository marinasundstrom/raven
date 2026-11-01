using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class ForCaptureCodeGenTests
{
    [Theory]
    [InlineData("let items = [|1, 2, 3|]")]
    [InlineData("let items = System.Collections.Generic.List<int>([1, 2, 3])")]
    [InlineData("let items: System.Collections.IEnumerable = [|1, 2, 3|]")]
    [InlineData("", "1..=3")]
    [InlineData("let values = [|1, 2, 3|]; let items = System.MemoryExtensions.AsSpan<int>(values)")]
    public void LoopBodyAndCallbackReadCurrentIteration(string setup, string collection = "items")
    {
        var source = $$"""
            import System.*
            public class Probe {
                static func Run() -> int {
                    {{setup}}
                    var total = 0
                    for item in {{collection}} {
                        total += (int)item
                        let read = () => (int)item
                        total += read()
                    }
                    return total
                }
            }
            """;
        Assert.Equal(12, Run(source));
    }

    [Fact]
    public void CapturedReferenceIterationIsInitializedBeforeFirstBodyRead()
    {
        Assert.Equal(6, Run("""
            import System.*
            public class Probe {
                static func Run() -> int {
                    let items = System.Collections.Generic.List<string>(["ab", "c"])
                    var total = 0
                    for item in items {
                        total += item.Length
                        let read = () => item.Length
                        total += read()
                    }
                    return total
                }
            }
            """));
    }

    private static int Run(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ForCapture", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        return (int)loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null)!;
    }
}
