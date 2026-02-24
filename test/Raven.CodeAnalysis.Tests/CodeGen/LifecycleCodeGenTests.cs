using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class LifecycleCodeGenTests
{
    [Fact]
    public void StaticInitAndFinally_EmitAndExecute()
    {
        const string code = """
import System.*
import System.Console.*

class Customer {
    static init {
        WriteLine("static-init")
    }

    init {
        WriteLine("init")
    }

    finally {
        WriteLine("final")
    }
}

class Program {
    static func CreateCustomer() -> unit {
        var customer = Customer()
    }

    static func Main() -> int {
        CreateCustomer()
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()
        0
    }
}
""";

        var output = CompileAndRun(code);

        Assert.True(output.Length >= 2, string.Join(Environment.NewLine, output));
        Assert.Equal("static-init", output[0]);
        Assert.Equal("init", output[1]);
        Assert.Contains("final", output);
    }

    [Fact]
    public void UnsafeLifecycleBlocks_EmitAndExecute()
    {
        const string code = """
import System.*
import System.Console.*

class UnsafeLifecycle {
    public static field StaticValue: int = 0
    public field Value: int = 0

    unsafe static init {
        var local = 7
        val pointer: *int = &local;
        StaticValue = *pointer
    }

    unsafe init {
        var local = 11
        val pointer: *int = &local;
        Value = *pointer
    }

    unsafe finally {
        var local = 13
        val pointer: *int = &local;
        WriteLine("ufinal:13")
    }
}

class Program {
    static func CreateValue() -> unit {
        var value = UnsafeLifecycle()
        WriteLine(UnsafeLifecycle.StaticValue)
        WriteLine(value.Value)
    }

    static func Main() -> int {
        CreateValue()
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()
        0
    }
}
""";

        var output = CompileAndRun(code);

        Assert.True(output.Length >= 3, string.Join(Environment.NewLine, output));
        Assert.Equal("7", output[0]);
        Assert.Equal("11", output[1]);
        Assert.Contains("ufinal:13", output);
    }

    private static string[] CompileAndRun(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var options = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithAllowUnsafe(true);

        var compilation = Compilation.Create("lifecycle_codegen", options)
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var writer = new StringWriter();
        var originalOut = Console.Out;

        try
        {
            Console.SetOut(writer);
            _ = entryPoint!.Invoke(null, entryPoint.GetParameters().Length == 0 ? null : [Array.Empty<string>()]);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString()
            .Replace("\r\n", "\n", StringComparison.Ordinal)
            .Split('\n', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
    }
}
