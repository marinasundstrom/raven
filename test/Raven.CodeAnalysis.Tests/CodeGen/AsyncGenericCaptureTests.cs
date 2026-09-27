using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class AsyncGenericCaptureTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task GenericMethodCaptureSharesReplacementAcrossAwait(bool arrayCapture)
    {
        var compilation = Compilation.Create("GenericCapture", [SyntaxTree.ParseText($$"""
            import System.*
            import System.Threading.Tasks.*
            public class Example {
                static async func Read<T>(initial: T, replacement: T) -> Task<T> {
                    {{(arrayCapture ? "var value: T[] = [initial]" : "var value = initial")}}
                    let read: Func<T> = () => {{(arrayCapture ? "value[0]" : "value")}}
                    await Task.Yield()
                    {{(arrayCapture ? "value[0]" : "value")}} = replacement
                    return read()
                }
            }
            """)], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var emitted = compilation.Emit(output);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(output, compilation.References);
        var method = loaded.Assembly.GetType("Example")!.GetMethod("Read")!;
        Assert.Equal(42, await (Task<int>)method.MakeGenericMethod(typeof(int)).Invoke(null, [1, 42])!);
        Assert.Equal("after", await (Task<string>)method.MakeGenericMethod(typeof(string)).Invoke(null, ["before", "after"])!);
    }
}
