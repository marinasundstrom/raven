using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class TrailingBlockCodeGenTests
{
    [Fact]
    public void RepeatedTrailingBlocksInSameScope_EmitDistinctLambdaBodies()
    {
        const string code = """
class Runner {
    static func Run() -> string {
        val a = Use {
            "a"
        }

        val b = Use {
            "b"
        }

        return a + "," + b
    }

    static func Use(content: () -> string) -> string {
        return content()
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("a,b", output);
    }

    [Fact]
    public void RepeatedBuilderTrailingBlocksInSameScope_EmitDistinctLambdaBodies()
    {
        const string code = """
import System.*

class BuilderAttribute<T> : Attribute {}

class Node {
    init(value: string) {
        Value = value
    }

    val Value: string
}

class ViewBuilder {
    static func BuildExpression(value: string) -> Node {
        return Node(value)
    }

    static func BuildBlock(items: Node[]) -> Node {
        return items[0]
    }
}

class Runner {
    static func Run() -> string {
        val a = Use {
            "a"
        }

        val b = Use {
            "b"
        }

        return a.Value + "," + b.Value
    }

    static func Use([Builder<ViewBuilder>] content: () -> Node) -> Node {
        return content()
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("a,b", output);
    }

    [Fact]
    public void ParameterizedTrailingBlock_ParticipatesInOverloadResolution()
    {
        const string code = """
class Runner {
    static func Run() -> string {
        return Use { value =>
            value.ToString()
        }
    }

    static func Use(handler: () -> string) -> string {
        return "zero"
    }

    static func Use(handler: int -> string) -> string {
        return handler(42)
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("42", output);
    }

    private static object? CompileAndRun(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "trailing-block-codegen",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var programType = loaded.Assembly.GetType("Runner", throwOnError: true)!;
        var runMethod = programType.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static);
        Assert.NotNull(runMethod);

        return runMethod!.Invoke(null, Array.Empty<object?>());
    }
}
