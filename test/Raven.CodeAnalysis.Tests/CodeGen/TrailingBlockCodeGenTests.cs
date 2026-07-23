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
        self.Handler = handler
    }

    var Handler: T -> string

    func Run(value: T) -> string {
        return self.Handler(value)
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
