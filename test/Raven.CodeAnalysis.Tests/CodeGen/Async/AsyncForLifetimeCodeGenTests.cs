using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class AsyncForLifetimeCodeGenTests
{
    [Theory]
    [InlineData("finish")]
    [InlineData("continue")]
    [InlineData("break")]
    [InlineData("return")]
    [InlineData("cancel")]
    [InlineData("fault")]
    public async Task IteratorSurvivesPendingAwaitAndDisposesExactlyOnce(string exit)
    {
        const string source = """
import System.Collections.Generic.*
import System.Threading.Tasks.*
class Program {
    static async func Run(values: IEnumerable<int>, gate: Task<int>, exit: string) -> Task<int> {
        var sum = 0
        for item in values {
            let value = await gate
            sum = sum + item + value
            if exit == "return" { return sum }
            if exit == "break" { break }
            if exit == "continue" { continue }
        }
        return sum
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("async-for-lifetime", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var gate = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        var values = new Probe();
        var task = Assert.IsAssignableFrom<Task<int>>(loaded.Assembly.GetType("Program")!.GetMethod("Run")!
            .Invoke(null, [values, gate.Task, exit]));
        Assert.False(task.IsCompleted, task.Exception?.ToString());
        Assert.Equal(1, values.Acquisitions);
        Assert.Equal(1, values.Reads);
        Assert.Equal(0, values.Disposals);
        if (exit == "cancel") gate.SetCanceled();
        else if (exit == "fault") gate.SetException(new InvalidOperationException("gate fault"));
        else gate.SetResult(40);
        if (exit == "cancel")
            await Assert.ThrowsAnyAsync<OperationCanceledException>(() => task.WaitAsync(TimeSpan.FromSeconds(10)));
        else if (exit == "fault")
            await Assert.ThrowsAsync<InvalidOperationException>(() => task.WaitAsync(TimeSpan.FromSeconds(10)));
        else
            Assert.Equal(exit is "break" or "return" ? 41 : 83, await task.WaitAsync(TimeSpan.FromSeconds(10)));
        Assert.Equal(1, values.Acquisitions);
        Assert.Equal(1, values.Disposals);
        Assert.Equal(exit is "finish" or "continue" ? 2 : 1, values.Reads);
    }

    [Theory]
    [InlineData("[1, 2]", "IEnumerable<int>")]
    [InlineData("1..2", "IEnumerable<int>")]
    [InlineData("2..1 by -1", "IEnumerable<int>")]
    [InlineData("1..<3", "IEnumerable<int>")]
    [InlineData("values", "List<int>")]
    [InlineData("values", "int[]")]
    public async Task LoopStateSurvivesTwoPendingAwaits(string collection, string parameterType)
    {
        var source = $$"""
import System.*
import System.Collections.Generic.*
import System.Threading.Tasks.*
class Program {
    static async func Run(values: {{parameterType}}, gates: Task<int>[], entered: Action<int>) -> Task<int> {
        var index = 0
        var sum = 0
        for item in {{collection}} {
            entered(index)
            let value = await gates[index]
            sum = sum + item + value
            index = index + 1
        }
        return sum
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("async-for-state", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var first = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        var second = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        var secondEntered = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);
        Action<int> entered = i => { if (i == 1) secondEntered.TrySetResult(true); };
        var task = Assert.IsAssignableFrom<Task<int>>(loaded.Assembly.GetType("Program")!.GetMethod("Run")!
            .Invoke(null, [parameterType == "int[]" ? new int[] { 1, 2 } : new List<int> { 1, 2 },
                new[] { first.Task, second.Task }, entered]));
        try
        {
            Assert.False(task.IsCompleted, task.Exception?.ToString());
            first.SetResult(40);
            await secondEntered.Task.WaitAsync(TimeSpan.FromSeconds(10));
            Assert.False(task.IsCompleted, task.Exception?.ToString());
            second.SetResult(40);
            Assert.Equal(83, await task.WaitAsync(TimeSpan.FromSeconds(10)));
        }
        finally
        {
            first.TrySetResult(40);
            second.TrySetResult(40);
        }
    }

    [Theory]
    [InlineData("continue", 80, 2, false)]
    [InlineData("continue", 80, 2, true)]
    [InlineData("break", 40, 1, false)]
    [InlineData("break", 40, 1, true)]
    public async Task OutwardLoopTransferDisposesInnerIterator(string transfer, int expected, int acquisitions, bool useResource)
    {
        var source = $$"""
import System.Collections.Generic.*
import System.Threading.Tasks.*
class Program {
    static async func Run(values: IEnumerable<int>, gate: Task<int>, cleanup: System.IDisposable) -> Task<int> {
        var sum = 0
        outer: for index in 1..2 {
            for item in values {
                {{(useResource ? "use resource = cleanup" : "")}}
                let value = await gate
                sum = sum + value
                {{transfer}} outer
            }
        }
        return sum
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("async-for-transfer", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var gate = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        var values = new Probe();
        var cleanup = new Probe();
        var task = Assert.IsAssignableFrom<Task<int>>(loaded.Assembly.GetType("Program")!.GetMethod("Run")!
            .Invoke(null, [values, gate.Task, cleanup]));
        Assert.False(task.IsCompleted, task.Exception?.ToString());
        Assert.Equal(0, values.Disposals);
        Assert.Equal(0, cleanup.Disposals);
        gate.SetResult(40);
        Assert.Equal(expected, await task.WaitAsync(TimeSpan.FromSeconds(10)));
        Assert.Equal(acquisitions, values.Acquisitions);
        Assert.Equal(acquisitions, values.Disposals);
        Assert.Equal(useResource ? acquisitions : 0, cleanup.Disposals);
    }

    [Fact]
    public async Task IteratorIsDisposedBeforeFollowingAwait()
    {
        const string source = """
import System.Collections.Generic.*
import System.Threading.Tasks.*
class Program {
    static async func Run(values: IEnumerable<int>, first: Task<int>, second: Task<int>) -> Task<int> {
        for item in values {
            let value = await first
        }
        return await second
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("async-for-scope", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var second = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        var values = new Probe();
        var task = Assert.IsAssignableFrom<Task<int>>(loaded.Assembly.GetType("Program")!.GetMethod("Run")!
            .Invoke(null, [values, Task.FromResult(1), second.Task]));
        Assert.False(task.IsCompleted, task.Exception?.ToString());
        Assert.Equal(1, values.Disposals);
        second.SetResult(42);
        Assert.Equal(42, await task.WaitAsync(TimeSpan.FromSeconds(10)));
        Assert.Equal(1, values.Disposals);
    }

    private sealed class Probe : IEnumerable<int>, IEnumerator<int>
    {
        public int Acquisitions, Disposals, Reads;
        private int index;
        public IEnumerator<int> GetEnumerator() { ++Acquisitions; index = 0; return this; }
        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        public int Current { get { ++Reads; return index; } }
        object IEnumerator.Current => Current;
        public bool MoveNext() => ++index <= 2;
        public void Dispose() => ++Disposals;
        public void Reset() => throw new NotSupportedException();
    }
}
