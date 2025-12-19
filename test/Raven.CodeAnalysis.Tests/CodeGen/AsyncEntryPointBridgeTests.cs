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
}
