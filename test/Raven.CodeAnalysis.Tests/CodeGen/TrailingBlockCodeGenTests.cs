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

    [Fact]
    public void ExplicitlyTypedTrailingBlock_InfersGenericConstructorLikeRegularLambdaArgument()
    {
        const string code = """
class GET {
    init(pattern: string, handler: () -> string) {
    }

    func Run() -> string {
        return "zero"
    }
}

class GET<T> {
    init(pattern: string, handler: T -> string) {
        Handler = handler
    }

    val Handler: T -> string

    func Run(value: T) -> string {
        return Handler(value)
    }
}

class Runner {
    static func Run() -> string {
        val trailing = GET("/{id:int}") { (id: int) =>
            id.ToString()
        }

        val regular = GET("/{id:int}", (id: int) => id.ToString())

        return trailing.Run(42) + "," + regular.Run(42)
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("42,42", output);
    }

    [Fact]
    public void BuilderTrailingBlock_DoesNotHoistNestedTrailingBlockParameter()
    {
        const string code = """
import System.*

class BuilderAttribute<T> : Attribute {}

open class Node {}

class ListNode : Node {
    init(items: Node[]) {
    }
}

class DslBuilder {
    static func BuildExpression(node: Node) -> Node {
        return node
    }

    static func BuildBlock(items: Node[]) -> Node {
        return ListNode(items)
    }
}

class GET<T> : Node {
    init(pattern: string, handler: T -> string) {
        Handler = handler
    }

    val Handler: T -> string
}

class Store {
    func Find(id: int) -> string? {
        if id == 42 {
            return "found"
        }

        return null
    }
}

class Runner {
    static func Run() -> string {
        val store = Store()
        val node = Route() {
            GET("/{id:int}") { (id: int) =>
                val todo = store.Find(id)
                if todo is null {
                    return "Not found"
                } else {
                    return todo
                }
            }
        }

        return "ok"
    }

    static func Route([Builder<DslBuilder>] endpoints: () -> Node) -> Node {
        return endpoints()
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("ok", output);
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
