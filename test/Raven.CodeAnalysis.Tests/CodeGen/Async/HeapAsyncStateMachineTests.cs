using System;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class HeapAsyncStateMachineTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public async Task PendingStateRetainsLocalsAfterKickoffReturns(bool heap, bool completed)
    {
        const string source = """
import System.Threading.Tasks.*
class Program {
    static async func Run(first: Task<int>, second: Task<int>) -> Task<int> {
        let seed = 40
        let left = await first
        let right = await second
        return seed + left + right
    }
}
""";
        var references = TestMetadataReferences.Default;
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithAsyncExceptionCapture(false).WithHeapAsyncStateMachines(heap);
        var compilation = Compilation.Create("heap-async", options)
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Program")!.GetMethod("Run")!;
        var stateMachine = method.GetCustomAttribute<AsyncStateMachineAttribute>()!.StateMachineType;
        Assert.Equal(!heap, stateMachine.IsValueType);
        var first = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        var second = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        if (completed) { first.SetResult(1); second.SetResult(1); }
        var task = Assert.IsAssignableFrom<Task<int>>(method.Invoke(null, [first.Task, second.Task]));
        if (!completed) Assert.False(task.IsCompleted);
        GC.Collect();
        GC.WaitForPendingFinalizers();
        first.TrySetResult(1);
        second.TrySetResult(1);
        Assert.Equal(42, await task.WaitAsync(TimeSpan.FromSeconds(10)));
    }

    [Fact]
    public async Task AwaitlessHeapMethodCompletesThroughBuilder()
    {
        const string source = """
import System.Threading.Tasks.*
class Program {
    static async func Run() -> Task<int> { return 42 }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("heap-awaitless",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithHeapAsyncStateMachines(true))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Program")!.GetMethod("Run")!;
        Assert.False(method.GetCustomAttribute<AsyncStateMachineAttribute>()!.StateMachineType.IsValueType);
        var task = Assert.IsAssignableFrom<Task<int>>(method.Invoke(null, null));
        Assert.Equal(42, await task.WaitAsync(TimeSpan.FromSeconds(10)));
    }

    [Fact]
    public void CopiesRetainHeapPolicyAndDefaultRemainsValueType()
    {
        Assert.False(new CompilationOptions().UseHeapAsyncStateMachines);
        var options = new CompilationOptions().WithHeapAsyncStateMachines(true)
            .WithAsyncExceptionCapture(false).WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithGraphemeChar(false).WithOptimizationLevel(OptimizationLevel.Release);
        Assert.True(options.UseHeapAsyncStateMachines);
        Assert.False(options.CaptureAsyncExceptions);
        Assert.False(options.WithHeapAsyncStateMachines(false).UseHeapAsyncStateMachines);
    }
}
