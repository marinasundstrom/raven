using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class NullLiteralEmissionTests
{
    [Fact]
    public void NullableReference_ReturningNull_EmitsNull()
    {
        const string code = """
class C {
    Run() -> string? {
        return null
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;

        var value = method.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);
    }

    [Fact]
    public void NullableValue_ReturningNull_EmitsDefault()
    {
        const string code = """
class C {
    Run() -> int? {
        return null
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;

        var value = method.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);
    }

    [Fact]
    public void NullableValue_LocalInitializedWithNull_ReturnsDefault()
    {
        const string code = """
class C {
    Run() -> int? {
        let value: int? = null
        return value
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run")!;

        var value = method.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);
    }
}
