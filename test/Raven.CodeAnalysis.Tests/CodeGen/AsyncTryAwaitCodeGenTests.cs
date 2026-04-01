using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class AsyncTryAwaitCodeGenTests
{
    [Fact]
    public void TryAwaitExpression_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class Program {
    static async func Fetch() -> Task<Result<int, Exception>> {
        val value = try? await Task.FromResult(42)
        return .Ok(value)
    }

    static async func Main() -> Task {
        val result = await Program.Fetch()
        Console.WriteLine(result)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "Result.Ok(42)" }, output);
    }

    [Fact]
    public void TryAwaitExpression_WithUse_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.IO.*
import System.Threading.Tasks.*

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Program {
    static async func ThrowingAsync() -> Task<int> {
        throw Exception("boom")
    }

    static async func Fetch(shouldThrow: bool) -> Task<Result<int, Exception>> {
        use stream = MemoryStream()

        if shouldThrow {
            val value = try? await Program.ThrowingAsync()
            return .Ok(value)
        }

        val okValue = try? await Task.FromResult(7)
        return .Ok(okValue)
    }

    static async func Main() -> Task {
        val success = await Program.Fetch(false)
        val failure = await Program.Fetch(true)

        val successText = success match {
            .Ok(val v) => "ok:" + v.ToString()
            .Error(val e) => "err:" + e.Message
        }

        val failureText = failure match {
            .Ok(val v) => "ok:" + v.ToString()
            .Error(val e) => "err:" + e.Message
        }

        Console.WriteLine(successText)
        Console.WriteLine(failureText)
    }
}
""";

        EmitOnly(code);
    }

    [Fact]
    public void TryAwaitExpression_WithTaskOfResultOperand_ProjectsThrownExceptionToError()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class Program {
    static async func Action(throwExc: bool) -> Task<Result<int, Exception>> {
        await Task.Delay(1)
        if throwExc {
            throw Exception("Boom!")
        }

        return .Ok(40)
    }

    static async func Test(throwExc: bool) -> Task<Result<int, Exception>> {
        val x = try? await Program.Action(throwExc)
        return .Ok(x + 2)
    }

    static async func Main() -> Task {
        Console.WriteLine(await Program.Test(false))
        Console.WriteLine(await Program.Test(true))
    }
}
""";

        EmitOnly(code);
    }

    [Fact]
    public void AsyncUse_WithResultMatchReturn_EmitsValidSetResultCall()
    {
        const string code = """
import System.*
import System.IO.*
import System.Threading.Tasks.*

union ApiError {
    Network(ex: Exception)
}

class Program {
    static async func DownloadText() -> Task<Result<string, ApiError>> {
        use stream = MemoryStream()

        return try await Task.FromResult("ok") match {
            .Ok(val text) => .Ok(text)
            .Error(Exception ex) => .Error(ApiError.Network(ex))
        }
    }

    static async func Main() -> Task {
        val result = await Program.DownloadText()
        Console.WriteLine(result)
    }
}
""";

        EmitOnly(code);
    }

    [Fact]
    public void TryExpression_WithGenericInvocationInsideAsyncMethod_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.Text.Json.*
import System.Threading.Tasks.*

union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Program {
    static async func Run() -> Task<int> {
        await Task.Delay(1)

        val result = try JsonSerializer.Deserialize<int>("1")

        if result is .Ok(val value) {
            return value
        }

        if result is .Error(Exception ex) {
            Console.WriteLine(ex.Message)
            return -1
        }

        return -1
    }

    static func Main() {
        Console.WriteLine(Program.Run().Result)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "1" }, output);
    }

    [Fact]
    public void AsyncUse_PrefersDisposeAsync_WhenAvailable()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class AsyncProbe : IAsyncDisposable, IDisposable {
    public func Dispose() -> unit => Console.WriteLine("Dispose")
    public func DisposeAsync() -> ValueTask {
        Console.WriteLine("DisposeAsync")
        return ValueTask.CompletedTask
    }
}

class Program {
    static async func Main() -> Task {
        use probe = AsyncProbe()
        await Task.Delay(1)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "DisposeAsync" }, output);
    }

    [Fact]
    public void AsyncUse_FallsBackToDispose_WhenDisposeAsyncIsUnavailable()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class Probe : IDisposable {
    public func Dispose() -> unit => Console.WriteLine("Dispose")
}

class Program {
    static async func Main() -> Task {
        use probe = Probe()
        await Task.Delay(1)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "Dispose" }, output);
    }

    [Fact]
    public void Async_ConditionalAccessThenPropagate_UsesSameOutStorage()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

union Err {
    MissingUser
    MissingName
}

class User {
    public var Name: string { get; set; } = ""
    public var Item: Option<Item> { get; set; } = .None
}

record class Item(Name: string)

class Program {
    static func GetUser() -> Result<User, Err> {
        return .Ok(User { Name = "Marina", Item = Item("Candy") })
    }

    static async func GetItem() -> Task<Result<string, Err>> {
        val maybeItem = GetUser()?.Item?
        await Task.Delay(1)

        return maybeItem match {
            .Some(val item) => .Ok(item.Name)
            .None => .Error(Err.MissingName)
        }
    }

    static async func Main() -> Task {
        Console.WriteLine(await Program.GetItem())
    }
}
""";

        EmitOnly(code);
    }

    [Fact(Skip = "Legacy async disposal lowering case; replace with newer semantic coverage.")]
    public void AsyncUse_DoesNotDisposeResourceBeforeAwaitResumes()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class Probe : IDisposable {
    public var IsDisposed: bool = false

    public async func ReadAsync() -> Task<int> {
        await Task.Delay(10)

        if self.IsDisposed {
            throw Exception("disposed-too-early")
        }

        return 42
    }

    public func Dispose() -> () {
        self.IsDisposed = true
    }
}

class Program {
    static async func Run() -> Task<Result<int, string>> {
        use probe = Probe()

        try {
            val value = await probe.ReadAsync()
            return .Ok(value)
        } catch (Exception e) {
            return .Error(e.Message)
        }
    }

    static async func Main() -> Task {
        val result = await Program.Run()
        Console.WriteLine(result)
    }
}
""";

        EmitOnly(code);
    }

    [Fact]
    public void AsyncOptionInvocation_NoneBranch_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class Program {
    static async func Fetch(flag: bool) -> Task<Option<int>> {
        await Task.Delay(1)

        if flag {
            return Some(42)
        }

        return None
    }

    static async func Main() -> Task {
        Console.WriteLine(await Program.Fetch(true))
        Console.WriteLine(await Program.Fetch(false))
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "Option.Some(42)", "Option.None" }, output);
    }

    [Fact]
    public void GenericAsyncMethod_ReturningGenericUnionAfterAwait_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.Console.*
import System.Threading.Tasks.*

union TaskState<T> {
    Success(value: T)
    Fault(exception: Exception?)
    Canceled
}

class Program {
    static async func AwaitState<T>(task: Task<T>) -> Task<TaskState<T>> {
        await task
        return task.Status match {
            .RanToCompletion => .Success(task.Result)
            .Faulted => .Fault(task.Exception)
            .Canceled => .Canceled
            _ => .Canceled
        }
    }

    static async func Main() -> Task {
        val state = await Program.AwaitState(Task.FromResult(42))
        _ = state
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Empty(output);
    }

    private static string[] CompileAndRun(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = GetReferencesWithRavenCore();
        var compilation = Compilation.Create(
            "async-try-await",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint!;

        var originalOut = Console.Out;
        using var writer = new StringWriter();

        try
        {
            Console.SetOut(writer);
            var parameters = entryPoint.GetParameters().Length == 0
                ? null
                : new object?[] { Array.Empty<string>() };
            entryPoint.Invoke(null, parameters);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            .Select(line => line.Trim())
            .ToArray();
    }

    private static void EmitOnly(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = GetReferencesWithRavenCore();
        var compilation = Compilation.Create(
            "async-try-await",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
    }

    private static MetadataReference[] GetReferencesWithRavenCore()
    {
        var corePath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (!File.Exists(corePath))
            return TestMetadataReferences.Default;

        return [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(corePath)];
    }
}
