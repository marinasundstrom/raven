using System;
using System.IO;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class UsingDeclarationCodeGenTests
{
    [Fact]
    public void TopLevelUsingDeclaration_DisposesAtEndOfScript()
    {
        var code = """
import System.*

using let foo = Foo()
foo.Do()

class Foo : IDisposable {
    public init() => Console.WriteLine("Init")
    public Do() => Console.WriteLine("Do")
    public Dispose() -> unit => Console.WriteLine("Dispose")
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
}
