using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class UseDeclarationCodeGenTests
{
    [Fact]
    public void TopLevelUseDeclaration_DisposesAtEndOfScript()
    {
        var code = """
import System.*

use foo = Foo()
foo.Do()

class Foo : IDisposable {
    public init() => Console.WriteLine("Init")
    public func Do() => Console.WriteLine("Do")
    public func Dispose() -> unit => Console.WriteLine("Dispose")
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "script",
                new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var entryPoint = assembly.EntryPoint!;

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

        Assert.Equal(new[] { "Init", "Do", "Dispose" }, output);
    }

    [Fact]
    public void UseDeclaration_WithInBlock_DisposesAtEndOfNestedBlock()
    {
        var code = """
import System.*

class Program {
    static func Run() -> unit {
        Console.WriteLine("Before")
        use foo = Foo { Value = 2 } in {
            Console.WriteLine(foo.Value)
        }
        Console.WriteLine("After")
    }
}

class Foo : IDisposable {
    public var Value: int = 0
    public init() {}
    public func Dispose() -> unit => Console.WriteLine("Dispose")
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create(
                "use_in_block",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Program", throwOnError: true)!;
        var run = type.GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;

        var originalOut = Console.Out;
        using var writer = new StringWriter();

        try
        {
            Console.SetOut(writer);
            run.Invoke(null, Array.Empty<object>());
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString()
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);

        Assert.Equal(new[] { "Before", "2", "Dispose", "After" }, output);
    }
}
