using System;
using System.IO;
using System.Reflection;

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
    static func WriteThrough() -> int {
        var value = 1
        val handle: &int = &value
        handle = 5
        value
    }
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
        var type = assembly.GetType("C", throwOnError: true)!;
        const BindingFlags flags = BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("WriteThrough", flags);
        Assert.NotNull(method);

        var value = (int)method!.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(5, value);
    }

    [Fact]
    public void ByRefParameter_AssignmentThroughAliasMutatesSource()
    {
        const string code = """
class Buffer {
    static func Write(var slot: &int, value: int) -> unit {
        slot = value
    }

    static func Run() -> int {
        var value = 10
        Write(&value, 42)
        value
    }
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
        var type = assembly.GetType("Buffer", throwOnError: true)!;
        const BindingFlags flags = BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);

        var value = (int)method!.Invoke(null, Array.Empty<object>())!;
        Assert.Equal(42, value);
    }
}
