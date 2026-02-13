using System;
using System.IO;
using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class AwaitReturnThrowExpressionCodeGenTests
{
    [Fact]
    public async Task AwaitNullCoalesceReturnExpression_ReturnsFromAsyncMethod()
    {
        var code = """
import System.Threading.Tasks.*

class Worker {
    public async LenOrNegativeOne(task: Task<string?>) -> Task<int> {
        val required = await task ?? return -1
        return required.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var workerType = loaded.Assembly.GetType("Worker", throwOnError: true)!;
        var worker = Activator.CreateInstance(workerType)!;
        var method = workerType.GetMethod("LenOrNegativeOne", BindingFlags.Public | BindingFlags.Instance)!;

        var nullTask = Task.FromResult<string?>(null);
        var valueTask = Task.FromResult<string?>("abcd");

        var nullResult = await InvokeTaskWithIntResultAsync(method, worker, nullTask);
        var valueResult = await InvokeTaskWithIntResultAsync(method, worker, valueTask);

        Assert.Equal(-1, nullResult);
        Assert.Equal(4, valueResult);
    }

    [Fact]
    public async Task AwaitNullCoalesceThrowExpression_ThrowsFromAsyncMethod()
    {
        var code = """
import System.Threading.Tasks.*

class Worker {
    public async LenOrThrow(task: Task<string?>) -> Task<int> {
        val required = await task ?? throw System.InvalidOperationException("missing")
        return required.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var workerType = loaded.Assembly.GetType("Worker", throwOnError: true)!;
        var worker = Activator.CreateInstance(workerType)!;
        var method = workerType.GetMethod("LenOrThrow", BindingFlags.Public | BindingFlags.Instance)!;

        var ex = await Assert.ThrowsAsync<InvalidOperationException>(async () =>
        {
            var task = (Task<int>)method.Invoke(worker, new object?[] { Task.FromResult<string?>(null) })!;
            await task.ConfigureAwait(false);
        });

        Assert.Equal("missing", ex.Message);
    }

    [Fact]
    public async Task AwaitInReturnExpression_NullCoalescePath_ReturnsAwaitedFallback()
    {
        var code = """
import System.Threading.Tasks.*

class Worker {
    private async Fallback(x: int) -> Task<int> {
        return x + 10
    }

    public async LenOrFallback(task: Task<string?>, x: int) -> Task<int> {
        val required = await task ?? return await Fallback(x)
        return required.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var workerType = loaded.Assembly.GetType("Worker", throwOnError: true)!;
        var worker = Activator.CreateInstance(workerType)!;
        var method = workerType.GetMethod("LenOrFallback", BindingFlags.Public | BindingFlags.Instance)!;

        var nullResult = await InvokeTaskWithIntResultAsync(method, worker, Task.FromResult<string?>(null), 5);
        var valueResult = await InvokeTaskWithIntResultAsync(method, worker, Task.FromResult<string?>("abcd"), 5);

        Assert.Equal(15, nullResult);
        Assert.Equal(4, valueResult);
    }

    private static async Task<int> InvokeTaskWithIntResultAsync(MethodInfo method, object instance, Task<string?> argument)
    {
        var task = (Task<int>)method.Invoke(instance, new object?[] { argument })!;
        return await task.ConfigureAwait(false);
    }

    private static async Task<int> InvokeTaskWithIntResultAsync(MethodInfo method, object instance, Task<string?> argument, int x)
    {
        var task = (Task<int>)method.Invoke(instance, new object?[] { argument, x })!;
        return await task.ConfigureAwait(false);
    }
}
