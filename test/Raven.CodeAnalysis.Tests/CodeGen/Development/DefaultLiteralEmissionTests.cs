using System.IO;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen.Development;

public class DefaultLiteralEmissionTests
{
    [Fact]
    public void DefaultLiteral_UnconstrainedTypeParameter_UsesInitobj()
    {
        const string code = """
class C {
    func Run<T>() -> T {
        return default
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

        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);
        var genericMethod = method!.MakeGenericMethod(typeof(int));
        var value = genericMethod.Invoke(instance, Array.Empty<object?>());

        Assert.Equal(0, value);

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void DefaultLiteral_ValueTypeConstraint_UsesInitobj()
    {
        const string code = """
class C {
    func RunStruct<T : struct>() -> T {
        return default
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

        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("RunStruct", flags);
        Assert.NotNull(method);
        var genericMethod = method!.MakeGenericMethod(typeof(int));
        var value = genericMethod.Invoke(instance, Array.Empty<object?>());

        Assert.Equal(0, value);

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void DefaultLiteral_ReferenceTypeConstraint_UsesLdnull()
    {
        const string code = """
class C {
    func RunClass<T : class>() -> T {
        return default
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

        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("RunClass", flags);
        Assert.NotNull(method);
        var genericMethod = method!.MakeGenericMethod(typeof(string));
        var value = genericMethod.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Ldnull, opcodes);
        Assert.DoesNotContain(OpCodes.Initobj, opcodes);
    }

    [Fact]
    public void DefaultExpression_UnconstrainedTypeParameter_UsesInitobj()
    {
        const string code = """
class C {
    func Run<T>() -> T {
        return default(T)
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

        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("Run", flags);
        Assert.NotNull(method);
        var genericMethod = method!.MakeGenericMethod(typeof(int));
        var value = genericMethod.Invoke(instance, Array.Empty<object?>());

        Assert.Equal(0, value);

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void DefaultExpression_ValueTypeConstraint_UsesInitobj()
    {
        const string code = """
class C {
    func RunStruct<T : struct>() -> T {
        return default(T)
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

        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("RunStruct", flags);
        Assert.NotNull(method);
        var genericMethod = method!.MakeGenericMethod(typeof(int));
        var value = genericMethod.Invoke(instance, Array.Empty<object?>());

        Assert.Equal(0, value);

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Initobj, opcodes);
        Assert.DoesNotContain(OpCodes.Ldnull, opcodes);
    }

    [Fact]
    public void DefaultExpression_ReferenceTypeConstraint_UsesLdnull()
    {
        const string code = """
class C {
    func RunClass<T : class>() -> T {
        return default(T)
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

        const BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        var method = type.GetMethod("RunClass", flags);
        Assert.NotNull(method);
        var genericMethod = method!.MakeGenericMethod(typeof(string));
        var value = genericMethod.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Ldnull, opcodes);
        Assert.DoesNotContain(OpCodes.Initobj, opcodes);
    }
}
