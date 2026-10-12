using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class AsyncReturnExpressionTests
{
    [Theory]
    [InlineData(false, 0, 42)]
    [InlineData(true, 0, 42)]
    [InlineData(false, 1, 3)]
    [InlineData(true, 1, 3)]
    public async Task MatchArmReturnCompletesTheOperation(bool suspend, int input, int expected)
    {
        var syntax = SyntaxTree.ParseText("""
            import System.Threading.Tasks.*
            public static class Consumer {
                public static async func Read(operation: Task<int>) -> Task<int> {
                    let value = match await operation {
                        0 => return 42
                        _ => 2
                    }
                    return value + 1
                }
            }
            """);
        var compilation = Compilation.Create("AsyncReturnExpression", [syntax], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var source = new TaskCompletionSource<int>();
        if (!suspend) source.SetResult(input);
        var operation = (Task<int>)loaded.Assembly.GetType("Consumer")!.GetMethod("Read")!.Invoke(null, [source.Task])!;
        if (suspend)
        {
            Assert.False(operation.IsCompleted);
            source.SetResult(input);
        }
        Assert.Equal(expected, await operation.WaitAsync(TimeSpan.FromSeconds(10)));
    }
}
