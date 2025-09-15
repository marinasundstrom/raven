using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ConditionalAccessTests
{
    [Fact]
    public void ConditionalAccess_NullReference_ReturnsNull()
    {
        var code = """
class Foo {
    Run() -> string? {
        var x: object? = null
        return x?.ToString()
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
        Assert.True(result.Success);
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = method.Invoke(instance, Array.Empty<object>());
        Assert.Null(value);
    }

    [Fact]
    public void ConditionalAccess_NullableValue_ReturnsValue()
    {
        var code = """
class Foo {
    Run() -> string? {
        var x: int? = 1
        return x?.ToString()
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
        Assert.True(result.Success);
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;
        var value = (string?)method.Invoke(instance, Array.Empty<object>());
        Assert.Equal("1", value);
    }
}
