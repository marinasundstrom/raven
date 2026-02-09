using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class ByRefCodeGenTests
{
    [Fact]
    public void ByRefLocal_AssignmentWritesThrough()
    {
        const string code = """
class C {
    static WriteThrough() -> int {
        var value = 1
        val handle: &int = &value
        handle = 5
        value
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", throwOnError: true)!;
        var method = type.GetMethod("WriteThrough")!;

        var value = (int)method.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(5, value);
    }

    [Fact]
    public void ByRefReturn_AssignmentThroughAliasMutatesSource()
    {
        const string code = """
class Buffer {
    static Identity(slot: &int) -> &int {
        return slot
    }

    static Run() -> int {
        var value = 10
        var slot = Identity(&value)
        slot = 42
        value
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Buffer", throwOnError: true)!;
        var method = type.GetMethod("Run")!;

        var value = (int)method.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(42, value);
    }
}
