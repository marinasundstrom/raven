using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class ExpressionBodyCodeGenTests
{
    [Fact]
    public void MethodAndConstructorExpressionBodies_AreEmitted()
    {
        var code = """
import System.*

class Foo : IDisposable {
    public init() => Console.WriteLine("Init")

    public Dispose() -> unit => Console.WriteLine("Dispose")
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var fooType = assembly.GetType("Foo", throwOnError: true)!;

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);

            var instance = Activator.CreateInstance(fooType);
            Assert.NotNull(instance);

            var dispose = fooType.GetMethod("Dispose", BindingFlags.Public | BindingFlags.Instance);
            Assert.NotNull(dispose);

            _ = dispose!.Invoke(instance, Array.Empty<object?>());
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

        Assert.Equal(new[] { "Init", "Dispose" }, output);
    }

    [Fact]
    public void FunctionExpressionBody_EmitsAndExecutes()
    {
        const string code = """
import System.*

func Foo(x: int) -> int => x + 1

func main() {
    Console.WriteLine(Foo(41))
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "function_expression_body",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        peStream.Position = 0;
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        try
        {
            Console.SetOut(writer);
            var parameters = entryPoint!.GetParameters().Length == 0
                ? Array.Empty<object?>()
                : new object?[] { Array.Empty<string>() };
            _ = entryPoint.Invoke(null, parameters);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

        Assert.Equal(new[] { "42" }, output);
    }
}
