using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class AsyncImplicitFieldReadTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public async Task CompoundFieldUpdatesUseOriginalReceiver(bool heap, bool completed)
    {
        var tree = SyntaxTree.ParseText("""
            import System.Threading.Tasks.*
            public class Consumer {
                private var remaining = 43
                public async func Read(gate: Task<int>) -> Task<int> {
                    let consumed = await gate
                    remaining -= consumed
                    return remaining
                }
            }
            """);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithHeapAsyncStateMachines(heap);
        var compilation = Compilation.Create("AsyncImplicitField", [tree], TestMetadataReferences.Default, options);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Consumer")!;
        var instance = Activator.CreateInstance(type);
        var gate = new TaskCompletionSource<int>();
        if (completed) gate.SetResult(1);
        var task = (Task<int>)type.GetMethod("Read")!.Invoke(instance, [gate.Task])!;
        if (!completed) { Assert.False(task.IsCompleted); gate.SetResult(1); }
        Assert.Equal(42, await task.WaitAsync(TimeSpan.FromSeconds(10)));
    }
}
