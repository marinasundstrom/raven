using System.IO;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class DefaultLiteralEmissionTests
{
    [Fact]
    public void DefaultLiteral_UnconstrainedTypeParameter_UsesInitobj()
    {
        const string code = """
class C {
    Run<T>() -> T {
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

        var method = type.GetMethod("Run")!;
        var genericMethod = method.MakeGenericMethod(typeof(int));
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
    RunStruct<T : struct>() -> T {
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

        var method = type.GetMethod("RunStruct")!;
        var genericMethod = method.MakeGenericMethod(typeof(int));
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
    RunClass<T : class>() -> T {
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

        var method = type.GetMethod("RunClass")!;
        var genericMethod = method.MakeGenericMethod(typeof(string));
        var value = genericMethod.Invoke(instance, Array.Empty<object?>());

        Assert.Null(value);

        var opcodes = ILReader.GetOpCodes(method);
        Assert.Contains(OpCodes.Ldnull, opcodes);
        Assert.DoesNotContain(OpCodes.Initobj, opcodes);
    }
}
