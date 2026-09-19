using System;
using System.IO;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class AsyncUnitResultTests
{
    [Theory]
    [InlineData("Task", "=> ()")]
    [InlineData("ValueTask", "=> ()")]
    [InlineData("Task", "{ return () }")]
    [InlineData("ValueTask", "{ return () }")]
    public async Task AwaitlessBody_ReturnsUnitPayload(string taskType, string body)
    {
        var source = $$"""
import System.Threading.Tasks.*
class Program {
    static async func Finish() -> {{taskType}}<unit> {{body}}
    static async func Run() -> Task<int> {
        let result: unit = await Finish()
        return 42
    }
}
""";
        var references = References();
        using var stream = Emit(source, references);
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var task = Assert.IsAssignableFrom<Task<int>>(loaded.Assembly.GetType("Program")!
            .GetMethod("Run")!.Invoke(null, null));
        Assert.Equal(42, await task.WaitAsync(TimeSpan.FromSeconds(10)));
    }

    [Theory]
    [InlineData("Task", false, "return ()")]
    [InlineData("Task", true, "return ()")]
    [InlineData("ValueTask", false, "return ()")]
    [InlineData("ValueTask", true, "return ()")]
    [InlineData("Task", false, "()")]
    [InlineData("ValueTask", false, "()")]
    public async Task UnitPayload_CompletesAfterAwait(string taskType, bool completed, string result)
    {
        var source = $$"""
import System.Threading.Tasks.*

class Program {
    static async func Finish(gate: Task<int>) -> {{taskType}}<unit> {
        await gate
        {{result}}
    }

    static async func Run(gate: Task<int>) -> Task<int> {
        let result: unit = await Finish(gate)
        return 42
    }
}
""";
        var references = References();
        using var stream = Emit(source, references);
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var gate = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        if (completed)
            gate.SetResult(1);
        var task = Assert.IsAssignableFrom<Task<int>>(loaded.Assembly.GetType("Program")!
            .GetMethod("Run")!.Invoke(null, [gate.Task]));
        try
        {
            Assert.Equal(completed, task.IsCompleted);
            gate.TrySetResult(1);
            Assert.Equal(42, await task.WaitAsync(TimeSpan.FromSeconds(10)));
        }
        finally
        {
            gate.TrySetResult(1);
        }
    }

    private static MetadataReference[] References() => [
        .. TestMetadataReferences.Default,
        MetadataReference.CreateFromFile(Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll"))
    ];

    private static MemoryStream Emit(string source, MetadataReference[] references)
    {
        var compilation = Compilation.Create("async-unit-result", [SyntaxTree.ParseText(source)],
            references, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var stream = new MemoryStream();
        var emit = compilation.Emit(stream);
        Assert.True(emit.Success, string.Join(Environment.NewLine, emit.Diagnostics));
        return stream;
    }

}
