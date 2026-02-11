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

    static async Main() -> Task {
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

        var programType = assembly.GetType("Program", throwOnError: true)!;
        var flagField = programType.GetField("Flag", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
        var flagValue = Assert.IsType<bool>(flagField!.GetValue(null));
        Assert.True(flagValue);
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
public union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Program {
    static Main() -> Result<int, string> {
        return .Error("Wrong args")
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
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
    public void ProgramMain_ReturningTaskOfResult_ErrorCasePayloadIsPrinted()
    {
        var code = """
import System.Threading.Tasks.*

public union Result<T, E> {
    Ok(value: T)
    Error(data: E)
}

class Program {
    static async Main(args: string[]) -> Task<Result<int, string>> {
        await Task.Yield()
        if args.Length == 0 {
            return .Error("boom")
        }

        return .Ok(args.Length)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
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
            var exitCode = entryPoint!.Invoke(null, new object?[] { Array.Empty<string>() });
            Assert.Equal(1, Assert.IsType<int>(exitCode));
            Assert.Contains("boom", errorWriter.ToString(), StringComparison.Ordinal);
        }
        finally
        {
            Console.SetError(originalError);
        }
    }

    [Fact]
    public void ProgramMain_TaskResultOfUnit_WithArgsAndAwait_EmitsAndRuns()
    {
        var code = """
import System.Threading.Tasks.*

public union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Program {
    static async Main(args: string[]) -> Task<Result<(), string>> {
        val first = args[0]
        await Task.Yield()

        if first.Length == 0 {
            return .Error("empty")
        }

        return .Ok
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
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

        var invokeResult = entryPoint!.Invoke(null, new object?[] { new[] { "ok" } });
        Assert.Null(invokeResult);
    }

    [Fact]
    public void ProgramMain_ReturningResultOfUnit_OkCaseDoesNotPrint()
    {
        var code = """
public union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Program {
    static Main(args: string[]) -> Result<(), string> {
        .Ok
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
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
            var invokeResult = entryPoint!.GetParameters().Length == 0
                ? entryPoint.Invoke(null, null)
                : entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });

            Assert.Null(invokeResult);
            Assert.Equal(string.Empty, errorWriter.ToString());
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
public union Result<T, E> {
    Ok(value: T)
    Error(error: E)
}

class Program {
    static Main(args: string[]) -> Result<int, string> {
        .Ok(42)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
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
