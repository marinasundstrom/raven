using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class ReturnExpressionCodeGenTests
{
    [Fact]
    public void ExpressionBlocks_WithReturnAndThrow_EmitAbruptControlFlow()
    {
        var code = """
import System.*

func LengthOrReturn(name: string?) -> int {
    let value = name ?? {
        return -1
    }
    return value.Length
}

func LengthOrThrow(name: string?) -> int {
    let value = name ?? {
        throw InvalidOperationException("missing")
    }
    return value.Length
}

func Main() {
    Console.WriteLine(LengthOrReturn(null))
    Console.WriteLine(LengthOrReturn("raven"))

    try {
        Console.WriteLine(LengthOrThrow(null))
    } catch Exception ex {
        Console.WriteLine(ex.Message)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "block-control-transfer-expressions", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var entryPoint = loaded.Assembly.EntryPoint!;
        var originalOut = Console.Out;
        using var writer = new StringWriter();

        try
        {
            Console.SetOut(writer);
            entryPoint.Invoke(null, entryPoint.GetParameters().Length == 0 ? null : [Array.Empty<string>()]);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

        Assert.Equal(["-1", "5", "missing"], output);
    }

    [Fact]
    public void NullCoalesce_ReturnExpression_PerformsEarlyReturn()
    {
        var code = """
import System.*

func Foo(name: string?) -> int {
    let value = name ?? return -1
    return value.Length
}

func Main() {
    Console.WriteLine(Foo(null))
    Console.WriteLine(Foo("abcd"))
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "return-expression", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

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

        var output = writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

        Assert.Equal(new[] { "-1", "4" }, output);
    }

    [Fact]
    public void IfStatement_ReturnExpressionsInBranches_EmitAndRun()
    {
        var code = """
import System.*

func Pick(flag: bool) -> int {
    if flag {
        return 1
    } else {
        return 0
    }
}

func Main() {
    Console.WriteLine(Pick(true))
    Console.WriteLine(Pick(false))
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "if-statement-return-expression", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

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

        var output = writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

        Assert.Equal(new[] { "1", "0" }, output);
    }
}
