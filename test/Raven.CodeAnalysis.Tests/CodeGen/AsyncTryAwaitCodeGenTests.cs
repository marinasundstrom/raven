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

async func Fetch() -> Task<System.Result<int, Exception>> {
    val value = try? await Task.FromResult(42)
    return .Ok(value)
}

class Program {
    static async Main() -> Task {
        val result = await Fetch()
        Console.WriteLine(result)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(new[] { "Result<Int32, Exception>.Ok(42)" }, output);
    }

    [Fact]
    public void TryAwaitExpression_WithUse_EmitsAndRuns()
    {
        const string code = """
import System.*
import System.IO.*
import System.Threading.Tasks.*

class Program {
    static async ThrowingAsync() -> Task<int> {
        throw Exception("boom")
    }

    static async Fetch(shouldThrow: bool) -> Task<System.Result<int, Exception>> {
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
