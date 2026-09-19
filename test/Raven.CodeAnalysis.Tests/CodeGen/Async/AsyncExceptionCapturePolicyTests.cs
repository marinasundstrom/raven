using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class AsyncExceptionCapturePolicyTests
{
    [Theory]
    [InlineData(false, false, "none")]
    [InlineData(false, false, "before")]
    [InlineData(false, false, "after")]
    [InlineData(false, true, "none")]
    [InlineData(false, true, "before")]
    [InlineData(false, true, "after")]
    [InlineData(true, false, "none")]
    [InlineData(true, false, "before")]
    [InlineData(true, false, "after")]
    [InlineData(true, true, "none")]
    [InlineData(true, true, "before")]
    [InlineData(true, true, "after")]
    public async Task PropagationBeforeAndAfterAwait_CompletesAsAnOrdinaryValue(
        bool capture, bool completed, string failure)
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Threading.Tasks.*

class Program {
    static func Value(value: int, fail: bool) -> Result<int, string> {
        if fail { return .Error("expected") }
        return .Ok(value)
    }

    static async func Fetch(gate: Task<int>, fail: bool) -> Task<Result<int, string>> {
        return Value(await gate, fail)
    }

    static async func Compute(trace: List<string>, gate: Task<int>, failure: string) -> Task<Result<int, string>> {
        trace.Add("entered")
        let seed = Value(1, failure == "before")?
        let value = (await Fetch(gate, failure == "after"))?
        trace.Add("continued")
        return .Ok(seed + value)
    }

    static async func Run(trace: List<string>, gate: Task<int>, failure: string) -> Task<string> {
        let result = await Compute(trace, gate, failure)
        return result match {
            .Ok(let value) => "ok:" + value.ToString()
            .Error(let error) => "error:" + error
        }
    }
}
""";
        var references = References();
        using var stream = Emit(source, capture, references);
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var program = loaded.Assembly.GetType("Program")!;
        var gate = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        var trace = new List<string>();
        if (completed)
            gate.SetResult(41);
        var task = Assert.IsAssignableFrom<Task<string>>(program.GetMethod("Run")!.Invoke(null, [trace, gate.Task, failure]));
        try
        {
            if (!completed && failure != "before")
                Assert.False(task.IsCompleted);
            if (failure == "before")
                Assert.True(task.IsCompleted);
            gate.TrySetResult(41);
            Assert.Equal(failure == "none" ? "ok:42" : "error:expected", await task.WaitAsync(TimeSpan.FromSeconds(10)));
            Assert.Equal(failure == "none" ? new[] { "entered", "continued" } : new[] { "entered" }, trace);

            var machines = program.GetNestedTypes(BindingFlags.NonPublic | BindingFlags.Public)
                .Select(type => type.GetMethod("MoveNext", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                .Where(method => method is not null).ToArray();
            Assert.NotEmpty(machines);
            foreach (var method in machines)
            {
                var handlers = method!.GetMethodBody()!.ExceptionHandlingClauses;
                if (capture)
                    Assert.NotEmpty(handlers);
                else
                    Assert.Empty(handlers);
            }
        }
        finally
        {
            gate.TrySetResult(41);
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public async Task UnrelatedUnionPayloads_UseOrdinaryCompletion(bool capture, bool completed)
    {
        const string source = """
import System.*
import System.Threading.Tasks.*

union Marker {
    case Ready(value: int)
}

class Program {
    static async func Read(gate: Task<int>) -> Task<Marker> {
        return .Ready(await gate)
    }
    static async func Run(gate: Task<int>) -> Task<string> {
        let marker = await Read(gate)
        return marker match { .Ready(let value) => value.ToString() }
    }
}
""";
        var references = References();
        using var stream = Emit(source, capture, references);
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var gate = new TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously);
        if (completed)
            gate.SetResult(42);
        var task = Assert.IsAssignableFrom<Task<string>>(loaded.Assembly.GetType("Program")!
            .GetMethod("Run")!.Invoke(null, [gate.Task]));
        try
        {
            if (!completed)
                Assert.False(task.IsCompleted);
            gate.TrySetResult(42);
            Assert.Equal("42", await task.WaitAsync(TimeSpan.FromSeconds(10)));
        }
        finally
        {
            gate.TrySetResult(42);
        }
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public async Task ExceptionCapture_IsExplicitAndDefaultsToDotNetBehavior(bool capture)
    {
        const string source = """
import System.*
import System.Threading.Tasks.*
class Program {
    static async func Run(gate: Task<int>) -> Task<int> {
        let value = await gate
        throw InvalidOperationException("failure")
    }
}
""";
        var references = References();
        using var stream = Emit(source, capture, references);
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Program")!.GetMethod("Run")!;
        if (capture)
        {
            var task = Assert.IsAssignableFrom<Task<int>>(method.Invoke(null, [Task.FromResult(1)]));
            var error = await Assert.ThrowsAsync<InvalidOperationException>(() => task);
            Assert.Equal("failure", error.Message);
        }
        else
        {
            // A .NET host models escape from the unguarded body. neoCLR has terminal Faults.
            var error = Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [Task.FromResult(1)]));
            Assert.Equal("failure", Assert.IsType<InvalidOperationException>(error.InnerException).Message);
        }
    }

    [Fact]
    public void OptionCopies_PreserveThePolicy()
    {
        var options = new CompilationOptions().WithAsyncExceptionCapture(false)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithGraphemeChar(false)
            .WithRuntimePropagationContract(null).WithRuntimeTypeOfContract(null)
            .WithOptimizationLevel(OptimizationLevel.Release);
        Assert.False(options.CaptureAsyncExceptions);
        Assert.True(options.WithAsyncExceptionCapture(true).CaptureAsyncExceptions);
        Assert.True(new CompilationOptions().CaptureAsyncExceptions);
    }

    private static MetadataReference[] References() =>
    [
        .. TestMetadataReferences.Default,
        MetadataReference.CreateFromFile(Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll"))
    ];

    private static MemoryStream Emit(string source, bool capture, MetadataReference[] references)
    {
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        // Exercise the unchanged default for .NET instead of explicitly enabling it.
        if (!capture)
            options = options.WithAsyncExceptionCapture(false);
        var compilation = Compilation.Create("async-capture-policy", options)
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        return stream;
    }
}
