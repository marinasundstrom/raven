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

    [Fact]
    public void FunctionPatterns_TestAndInvokeGenericStandardDelegates()
    {
        const string code = """
            import System.*

            func Read<T>(obj: object, fallback: T) -> T {
                if obj is () -> T {
                    if obj is () -> T callback {
                        return callback()
                    }
                }
                return fallback
            }
            func Match(obj: object) -> int {
                return obj match {
                    (int) -> int callback => callback(2)
                    _ => 0
                }
            }
            func Main() {
                let get: () -> int = () => 42
                let add: (int) -> int = value => value + 40
                Console.WriteLine(Read<int>(get, 0))
                Console.WriteLine(Read<string>(get, "mismatch"))
                Console.WriteLine(Read<int>("not a function", 7))
                Console.WriteLine(Match(add))
            }
            """;
        Assert.Equal(["42", "mismatch", "7", "42"], CompileAndRun(code));
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
