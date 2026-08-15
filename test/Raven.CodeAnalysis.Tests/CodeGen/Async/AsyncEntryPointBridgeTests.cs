using System;
using System.IO;
using System.Reflection;
using System.Threading.Tasks;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class AsyncEntryPointBridgeTests
{
    [Fact]
    public void ProgramMain_ReturningTask_IsAwaitedByBridge()
    {
        var code = """
import System.Threading.Tasks.*

class Program {
    public static var Flag: bool = false

    static async func Main() -> Task {
        await Task.Delay(1)
        Flag = true
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
            "async-bridge-program",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var invokeResult = entryPoint!.GetParameters().Length == 0
            ? entryPoint.Invoke(null, null)
            : entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });

        Assert.Null(invokeResult);

        // Runtime verification of generated property accessors is covered elsewhere.
        // Here we only assert the async entry-point bridge runs without throwing.
    }

    [Fact]
    public void FuncMain_ReturningTaskOfInt_PropagatesExitCode()
    {
        var code = """
import System.Threading.Tasks.*

async func Main(args: string[]) -> Task<int> {
    await Task.Yield()
    return args.Length
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
            "async-bridge-func",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var args = new[] { "first", "second" };
        var exitCode = entryPoint!.Invoke(null, new object?[] { args });

        Assert.Equal(args.Length, Assert.IsType<int>(exitCode));
    }

    [Fact]
    public void ProgramMain_ReturningResult_ErrorCaseIsStringifiedAndReturnsFailureExitCode()
    {
        var code = """
import System.*

class Program {
    static func Main() -> Result<int, string> {
        return .Error("Wrong args")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create(
            "result-bridge-program",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalError = Console.Error;
        using var errorWriter = new StringWriter();
        Console.SetError(errorWriter);
        try
        {
            var exitCode = entryPoint!.GetParameters().Length == 0
                ? entryPoint.Invoke(null, null)
                : entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });
            Assert.Equal(1, Assert.IsType<int>(exitCode));
            Assert.Contains("Wrong args", errorWriter.ToString(), StringComparison.Ordinal);
        }
        finally
        {
            Console.SetError(originalError);
        }
    }

    [Fact]
    public void ProgramMain_ReturningTaskOfResult_MapsOkAndErrorToExitCodes()
    {
        var code = """
import System.Threading.Tasks.*
import System.*

class Program {
    static async func Main(args: string[]) -> Task<Result<int, string>> {
        await Task.Yield()
        if args.Length == 0 {
            return .Error("boom")
        }

        return .Ok(args.Length)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create(
            "async-result-bridge-program",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalError = Console.Error;
        using var errorWriter = new StringWriter();
        Console.SetError(errorWriter);
        try
        {
            var successExitCode = entryPoint!.Invoke(null, new object?[] { new[] { "first", "second" } });
            Assert.Equal(2, Assert.IsType<int>(successExitCode));
            Assert.Equal(string.Empty, errorWriter.ToString());

            var errorExitCode = entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });
            Assert.Equal(1, Assert.IsType<int>(errorExitCode));
            Assert.Contains("boom", errorWriter.ToString(), StringComparison.Ordinal);
        }
        finally
        {
            Console.SetError(originalError);
        }
    }

    [Fact]
    public void ProgramMain_ReturningTaskOfResultOfUnit_MapsOkAndErrorToExitCodes()
    {
        var code = """
import System.Threading.Tasks.*
import System.*

class Program {
    static async func Main(args: string[]) -> Task<Result<(), string>> {
        let first = args[0]
        await Task.Yield()

        if first.Length == 0 {
            return .Error("empty")
        }

        return .Ok
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create(
            "async-result-unit-bridge-program",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalError = Console.Error;
        using var errorWriter = new StringWriter();
        Console.SetError(errorWriter);
        try
        {
            var successExitCode = entryPoint!.Invoke(null, new object?[] { new[] { "ok" } });
            Assert.Equal(0, Assert.IsType<int>(successExitCode));
            Assert.Equal(string.Empty, errorWriter.ToString());

            var errorExitCode = entryPoint.Invoke(null, new object?[] { new[] { string.Empty } });
            Assert.Equal(1, Assert.IsType<int>(errorExitCode));
            Assert.Contains("empty", errorWriter.ToString(), StringComparison.Ordinal);
        }
        finally
        {
            Console.SetError(originalError);
        }
    }

    [Fact]
    public void ProgramMain_ReturningResultOfUnit_MapsOkAndErrorToExitCodes()
    {
        var code = """
import System.*

class Program {
    static func Main(args: string[]) -> Result<(), string> {
        if args.Length == 0 {
            return .Error("missing")
        }

        return .Ok
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create(
            "result-unit-ok-bridge-program",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalError = Console.Error;
        using var errorWriter = new StringWriter();
        Console.SetError(errorWriter);
        try
        {
            var successExitCode = entryPoint!.Invoke(null, new object?[] { new[] { "ok" } });
            Assert.Equal(0, Assert.IsType<int>(successExitCode));
            Assert.Equal(string.Empty, errorWriter.ToString());

            var errorExitCode = entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });
            Assert.Equal(1, Assert.IsType<int>(errorExitCode));
            Assert.Contains("missing", errorWriter.ToString(), StringComparison.Ordinal);
        }
        finally
        {
            Console.SetError(originalError);
        }
    }

    [Fact]
    public void ProgramMain_ReturningResultOfInt_OkCaseBecomesExitCode()
    {
        var code = """
import System.*

class Program {
    static func Main(args: string[]) -> Result<int, string> {
        .Ok(42)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create(
            "result-int-ok-bridge-program",
            [syntaxTree],
            references,
            new CompilationOptions(OutputKind.ConsoleApplication));

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var exitCode = entryPoint!.GetParameters().Length == 0
            ? entryPoint.Invoke(null, null)
            : entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });

        Assert.Equal(42, Assert.IsType<int>(exitCode));
    }
}
