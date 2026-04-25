using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class PatternWholeDesignationCodeGenTests
{
    [Fact]
    public void IfPatternStatement_WithWholeDesignation_BindsMatchedTupleValue()
    {
        var code = """
import System.Console.*

func Main() {
    val point: (int, double) = (2, 1.5)
    if val (2, > 0.5) matched = point {
        WriteLine(matched.Item1)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["2"], output);
    }

    [Fact]
    public void ForPatternTarget_WithWholeDesignation_BindsIterationElement()
    {
        var code = """
import System.Console.*

val points: (int, double)[] = [(2, 1.0), (2, 0.25), (2, 3.0)]

for val (2, > 0.5) point in points {
    WriteLine(point.Item2)
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["1", "3"], output);
    }

    [Fact]
    public void IfPatternStatement_WithPropertyPattern_BindsCapturedMemberValue()
    {
        var code = """
import System.Console.*

record class Person(Name: string, Age: int)

class Program {
    static func Main() {
        val value: object = Person("Ada", 42)
        if val Person { Name: "Ada", Age: age } = value {
            WriteLine(age)
        }
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["42"], output);
    }

    [Fact]
    public void WhilePatternStatement_WithTuplePattern_BindsCapturedValue()
    {
        var code = """
import System.Console.*

class Program {
    static var index = 0

    static func Next() -> (string, int) {
        index = index + 1
        if index < 4 {
            return ("Ok", index)
        }

        return ("Done", 0)
    }

    static func Main() {
        var total = 0
        while val ("Ok", value) = Next() {
            total = total + value
        }

        WriteLine(total)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["6"], output);
    }

    [Fact]
    public void WhilePatternStatement_WithUnionCasePattern_BindsCapturedValue()
    {
        var code = """
import System.Console.*

union Result<T> {
    case Ok(value: T)
    case Done
}

class Program {
    static var index = 0

    static func Next() -> Result<int> {
        index = index + 1
        if index < 4 {
            return .Ok(index)
        }

        return .Done
    }

    static func Main() {
        var total = 0
        while val .Ok(value) = Next() {
            total = total + value
        }

        WriteLine(total)
    }
}
""";

        var output = CompileAndRun(code);
        Assert.Equal(["6"], output);
    }

    private static string[] CompileAndRun(string code, OutputKind outputKind = OutputKind.ConsoleApplication)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("pattern-whole-designation", new CompilationOptions(outputKind))
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
