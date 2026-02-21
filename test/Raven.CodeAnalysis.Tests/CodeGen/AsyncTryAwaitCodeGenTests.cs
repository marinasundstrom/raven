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
    static async Fetch() -> Task<Result<int, Exception>> {
        val value = try? await Task.FromResult(42)
        return .Ok(value)
    }

    static async Main() -> Task {
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
    static async ThrowingAsync() -> Task<int> {
        throw Exception("boom")
    }

    static async Fetch(shouldThrow: bool) -> Task<Result<int, Exception>> {
        use stream = MemoryStream()

        if shouldThrow {
            val value = try? await Program.ThrowingAsync()
            return .Ok(value)
        }

        val okValue = try? await Task.FromResult(7)
        return .Ok(okValue)
    }

    static async Main() -> Task {
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

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "ok:7", "err:boom" }, output);
    }

    [Fact]
    public void TryAwaitExpression_WithTaskOfResultOperand_ProjectsThrownExceptionToError()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class Program {
    static async Action(throwExc: bool) -> Task<Result<int, Exception>> {
        await Task.Delay(1)
        if throwExc {
            throw Exception("Boom!")
        }

        return .Ok(40)
    }

    static async Test(throwExc: bool) -> Task<Result<int, Exception>> {
        val x = try? await Program.Action(throwExc)
        return .Ok(x + 2)
    }

    static async Main() -> Task {
        Console.WriteLine(await Program.Test(false))
        Console.WriteLine(await Program.Test(true))
    }
}
""";

        var output = CompileAndRun(code);
        Assert.NotEmpty(output);
        Assert.Equal("Result.Ok(42)", output[0]);
        Assert.True(
            output.Skip(1).Any(line => line.StartsWith("Result.Error(System.Exception: Boom!", StringComparison.Ordinal)),
            "Expected thrown exception to be projected as Result.Error.");
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
    static async DownloadText() -> Task<Result<string, ApiError>> {
        use stream = MemoryStream()

        return try await Task.FromResult("ok") match {
            .Ok(val text) => .Ok(text)
            .Error(Exception ex) => .Error(ApiError.Network(ex))
        }
    }

    static async Main() -> Task {
        val result = await Program.DownloadText()
        Console.WriteLine(result)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "Result.Ok(\"ok\")" }, output);
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
    public Name: string { get; set; } = ""
    public Item: Option<Item> { get; set; } = .None
}

record class Item(Name: string)

class Program {
    static GetUser() -> Result<User, Err> {
        return .Ok(User { Name = "Marina", Item = Item("Candy") })
    }

    static async GetItem() -> Task<Result<string, Err>> {
        val maybeItem = GetUser()?.Item?
        await Task.Delay(1)

        return maybeItem match {
            .Some(val item) => .Ok(item.Name)
            .None => .Error(Err.MissingName)
        }
    }

    static async Main() -> Task {
        Console.WriteLine(await Program.GetItem())
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "Result.Ok(\"Candy\")" }, output);
    }

    [Fact]
    public void AsyncUse_DoesNotDisposeResourceBeforeAwaitResumes()
    {
        const string code = """
import System.*
import System.Threading.Tasks.*

class Probe : IDisposable {
    public var IsDisposed: bool = false

    public async ReadAsync() -> Task<int> {
        await Task.Delay(10)

        if self.IsDisposed {
            throw Exception("disposed-too-early")
        }

        return 42
    }

    public Dispose() -> () {
        self.IsDisposed = true
    }
}

class Program {
    static async Run() -> Task<Result<int, string>> {
        use probe = Probe()

        try {
            val value = await probe.ReadAsync()
            return .Ok(value)
        } catch (Exception e) {
            return .Error(e.Message)
        }
    }

    static async Main() -> Task {
        val result = await Program.Run()
        Console.WriteLine(result)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "Result.Ok(42)" }, output);
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

    private static MetadataReference[] GetReferencesWithRavenCore()
    {
        var corePath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (!File.Exists(corePath))
            return TestMetadataReferences.Default;

        return [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(corePath)];
    }
}
