using System.IO;
using System.Reflection;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class EncodedStringLiteralCodeGenTests
{
    [Fact]
    public void Utf8EncodedStringLiteral_EmitsExpectedBytes()
    {
        const string source = """
class C {
    static Get() -> byte[] {
        return "Pågen"u8
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("encoded-utf8", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("C", true)!;
        var method = type.GetMethod("Get", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        Assert.NotNull(method);
        var instance = Activator.CreateInstance(type)!;
        var bytes = (byte[])method!.Invoke(instance, Array.Empty<object?>())!;

        Assert.Equal(Encoding.UTF8.GetBytes("Pågen"), bytes);
    }

    [Fact]
    public void AsciiEncodedStringLiteral_EmitsExpectedBytes()
    {
        const string source = """
class C {
    static Get() -> byte[] {
        return "Hello"ascii
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("encoded-ascii", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("C", true)!;
        var method = type.GetMethod("Get", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        Assert.NotNull(method);
        var instance = Activator.CreateInstance(type)!;
        var bytes = (byte[])method!.Invoke(instance, Array.Empty<object?>())!;

        Assert.Equal(Encoding.ASCII.GetBytes("Hello"), bytes);
    }

    [Fact]
    public void Utf8EncodedStringLiterals_CanBeConcatenated()
    {
        const string source = """
class C {
    Get() -> byte[] {
        return "På"u8 + "gen"u8
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("encoded-concat", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("C", true)!;
        var method = type.GetMethod("Get", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
        Assert.NotNull(method);
        var instance = Activator.CreateInstance(type)!;
        var bytes = (byte[])method!.Invoke(instance, Array.Empty<object?>())!;

        Assert.Equal(Encoding.UTF8.GetBytes("Pågen"), bytes);
    }
}
