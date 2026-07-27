using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class IfPatternExpressionCodeGenTests
{
    [Fact]
    public void IfPatternExpression_ExecutesMatchingAndFallbackBranches()
    {
        const string code = """
import System.*

union Maybe {
    case Some(value: int)
    case None
}

func ValueOrZero(option: Maybe) -> int {
    return if let .Some(x) = option {
        x
    } else {
        0
    }
}

func Main() {
    Console.WriteLine(ValueOrZero(.Some(42)))
    Console.WriteLine(ValueOrZero(.None))
}
""";

        Assert.Equal(["42", "0"], CompileAndRun(code));
    }

    private static string[] CompileAndRun(string code)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create(
                "if-pattern-expression-runtime",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(SyntaxTree.ParseText(code))
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
            entryPoint.Invoke(null, null);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        return writer.ToString().Split(
            Environment.NewLine,
            StringSplitOptions.RemoveEmptyEntries);
    }
}
