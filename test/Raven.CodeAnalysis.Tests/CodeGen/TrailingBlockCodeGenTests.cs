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
    public void TrailingBlock_CanSkipOptionalParametersBeforeFinalClosure()
    {
        const string code = """
class Runner {
    static func Run() -> string {
        val first = StackPanel(spacing: 8) {
            "child"
        }

        val second = StackPanel()
        return first + "|" + second
    }

    static func StackPanel(orientation: string = "vertical", spacing: int = 0, content: (() -> string)? = null) -> string {
        return orientation + ":" + spacing.ToString() + ":" + (content?() ?? "none")
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("vertical:8:child|vertical:0:none", output);
    }

    [Fact]
    public void OptionalDoubleParameterDefault_EmitsWithoutCustomAttributeFailure()
    {
        const string code = """
class Runner {
    static func Run() -> string {
        if Scale() == 1.5 {
            return "ok"
        }

        return "bad"
    }

    static func Scale(value: double = 1.5) -> double {
        return value
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("ok", output);
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
    public void ReceiverTrailingBlock_EmitsReceiverMemberAccesses()
    {
        const string code = """
import System.*

class ReceiverAttribute : Attribute {}

class ConfigBuilder {
    var Name: string = ""
    var Activated: bool = false

    func Activate() -> () {
        Activated = true
    }
}

class Configuration {
    init(name: string, activated: bool) {
        Name = name
        Activated = activated
    }

    val Name: string
    val Activated: bool
}

class Runner {
    static func Run() -> string {
        val config = Config {
            Name = "Foo"
            Activate()
        }

        return config.Name + ":" + config.Activated.ToString()
    }

    static func Config([Receiver] configure: ConfigBuilder -> unit) -> Configuration {
        val builder = ConfigBuilder()
        configure(builder)
        return Configuration(builder.Name, builder.Activated)
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("Foo:True", output);
    }

    [Fact]
    public void ReceiverTrailingBlock_WorksForConstructorAndMemberInvocation()
    {
        const string code = """
import System.*

class ReceiverAttribute : Attribute {}

class ConfigBuilder {
    var Name: string = ""
}

class Configuration {
    init([Receiver] configure: ConfigBuilder -> unit) {
        val builder = ConfigBuilder()
        configure(builder)
        Name = builder.Name
    }

    val Name: string
}

class Factory {
    func Define([Receiver] configure: ConfigBuilder -> unit) -> Configuration {
        return Configuration(configure)
    }
}

class Runner {
    static func Run() -> string {
        val constructed = Configuration {
            Name = "ctor"
        }

        val factory = Factory()
        val defined = factory.Define {
            Name = "member"
        }

        return constructed.Name + "," + defined.Name
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("ctor,member", output);
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

    [Fact]
    public void BuilderReceiverTrailingBlock_EmitsReceiverConfigurationAndComponents()
    {
        const string code = """
import System.*

class BuilderAttribute<T> : Attribute {}
class ReceiverAttribute<T> : Attribute {}

class Node {
    init(markup: string) {
        Markup = markup
    }

    val Markup: string
}

class UiBuilder {
    static func BuildExpression(node: Node) -> Node {
        return node
    }

    static func BuildBlock(items: Node[]) -> Node {
        var markup = ""

        for item in items {
            markup = markup + item.Markup
        }

        return Node(markup)
    }

    static func BuildFinalResult(content: Node, receiver: WindowBuilder) -> Node {
        return receiver.Build(content)
    }
}

class WindowBuilder {
    var Title: string = ""
    var Activated: bool = false

    func Activate() -> () {
        Activated = true
    }

    func Build(content: Node) -> Node {
        return Node(Title + ":" + Activated.ToString() + ":" + content.Markup)
    }
}

func Window([Builder<UiBuilder>, Receiver<WindowBuilder>] content: () -> Node) -> Node {
    return content()
}

func Text(value: string) -> Node {
    return Node(value)
}

class Runner {
    static func Run() -> string {
        val danger = true
        val view = Window {
            Title = "Tasks"
            Activate()

            if danger {
                Title = "DANGER!"
            }

            Text("Inbox")
        }

        return view.Markup
    }
}
""";

        var output = CompileAndRun(code);

        Assert.Equal("DANGER!:True:Inbox", output);
    }

    [Fact]
    public void TopLevelReturnScanner_IgnoresReturnsInsideTrailingBlock()
    {
        const string code = """
class UseBlock {
    init(handler: () -> string) {
        Value = handler()
    }

    val Value: string
}

val used = UseBlock {
    return "nested"
}
""";

        using var loaded = CompileConsoleApplication(code);
        var entryPoint = loaded.Assembly.EntryPoint!;

        Assert.Equal(typeof(void), entryPoint.ReturnType);
        entryPoint.Invoke(null, new object?[] { Array.Empty<string>() });
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

    private static TestAssemblyLoader.LoadedAssembly CompileConsoleApplication(string code)
    {
        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "trailing-block-console-codegen",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        return TestAssemblyLoader.LoadFromStream(peStream, references);
    }
}
