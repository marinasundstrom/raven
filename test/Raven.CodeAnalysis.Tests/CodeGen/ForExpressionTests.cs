using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class ForExpressionTests
{
    [Fact]
    public void ForEach_OverArray_UsesTypedElement()
    {
        var code = """
class Foo {
    Run() -> int {
        var items = [1, 2, 3]
        var total: int = 0
        for each item in items {
            total = total + item
        }
        return total
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (int)method.Invoke(instance, Array.Empty<object>())!;
        Assert.Equal(6, value);
    }

    [Fact]
    public void ForRange_IsInclusive_ForLiteralAndVariableBounds()
    {
        var code = """
import System.Console.*

for x in 2..6 {
    WriteLine(x)
}

val bottom = -3
val top = 7

for x in bottom..top {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(
            ["2", "3", "4", "5", "6", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7"],
            output);
    }

    [Fact]
    public void ForRange_SupportsCharAndDecimalBounds()
    {
        var code = """
import System.Console.*

for c in 'a'..'c' {
    WriteLine(c)
}

val start: decimal = 1
val end: decimal = 3

for n in start..end {
    WriteLine(n)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["a", "b", "c", "1", "2", "3"], output);
    }

    [Fact]
    public void ForRange_ByClause_SupportsPositiveAndNegativeIntegerSteps()
    {
        var code = """
import System.Console.*

for x in 0..10 by 2 {
    WriteLine(x)
}

for x in 10..0 by -3 {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["0", "2", "4", "6", "8", "10", "10", "7", "4", "1"], output);
    }

    [Fact]
    public void ForRange_ByClause_SupportsFractionalStep()
    {
        var code = """
import System.Console.*

for x in 0..1.0 by 0.5 {
    WriteLine(x)
}
""";

        var output = CompileAndRun(code);
        var normalized = output.Select(static value => value.Replace(',', '.')).ToArray();
        Assert.Equal(["0", "0.5", "1"], normalized);
    }

    private static string[] CompileAndRun(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("for-range", new CompilationOptions(OutputKind.ConsoleApplication))
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

        return writer.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
    }
}
