using System.IO;
using System.Reflection;
using System.Reflection.Emit;

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

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Ldnull, opcodes);
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

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void NullableValue_LocalInitializedWithNull_ReturnsDefault()
    {
        const string code = """
class C {
    Run() -> int? {
        val value: int? = null
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

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void NullableReference_LocalInitializedWithNull_DoesNotBox()
    {
        const string code = """
class C {
    Run() -> Action<int>? {
        val f: Action<int>? = null
        return f
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

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Ldnull, opcodes);
        Assert.DoesNotContain(OpCodes.Box, opcodes);
    }

    [Fact]
    public void NullableValue_EqualityAndInequality_WithNonNullableValue_EmitAndRun()
    {
        const string code = """
class C {
    EqNull() -> bool {
        val x: int? = null
        return x == 1
    }

    EqValue() -> bool {
        val x: int? = 1
        return x == 1
    }

    NeqNull() -> bool {
        val x: int? = null
        return x != 1
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
        Assert.Empty(compilation.GetDiagnostics());

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("C", true)!;
        var instance = Activator.CreateInstance(type)!;
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        var eqNullMethod = type.GetMethod("EqNull", flags);
        var eqValueMethod = type.GetMethod("EqValue", flags);
        var neqNullMethod = type.GetMethod("NeqNull", flags);

        Assert.NotNull(eqNullMethod);
        Assert.NotNull(eqValueMethod);
        Assert.NotNull(neqNullMethod);

        var eqNull = (bool)eqNullMethod!.Invoke(instance, Array.Empty<object?>())!;
        var eqValue = (bool)eqValueMethod!.Invoke(instance, Array.Empty<object?>())!;
        var neqNull = (bool)neqNullMethod!.Invoke(instance, Array.Empty<object?>())!;

        Assert.False(eqNull);
        Assert.True(eqValue);
        Assert.True(neqNull);
    }
}
