using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class GenericInvocationCodeGenTests
{
    [Fact]
    public void UnconstrainedTypeParameter_ToString_BoxesValueTypes()
    {
        const string code = """
import System.*

class Formatter {
    func Format<T>(value: T) -> string {
        return value.ToString()
    }

    func Run() -> string {
        return Format<int>(42)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("generic_invocation", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Formatter", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(method);
        var value = (string?)method!.Invoke(instance, Array.Empty<object>());
        Assert.Equal("42", value);
    }

    [Fact]
    public void StructConstrainedVarArgs_ConvertsElementToObject_ExecutesWithoutInvalidProgram()
    {
        const string code = """
class Formatter {
    static func Consume(value: object) -> int {
        return 1
    }

    static func Collect<T>(items: T ...) where T: struct {
        for item in items {
            Consume(item)
        }
    }

    func Run() -> int {
        val arr: int[] = [1, 2, 3]
        Collect(arr)
        return 1
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("generic_varargs_boxing", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Formatter", true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(method);
        var value = (int?)method!.Invoke(instance, Array.Empty<object>());
        Assert.Equal(1, value);
    }

    [Fact]
    public void StructConstrainedVarArgs_SingleSpread_DoesNotMaterializeIntermediateList()
    {
        const string code = """
import System.Collections.Immutable.*

class Formatter {
    static func Collect<T>(items: T ...) where T: struct {
        for item in items {
            item.ToString()
        }
    }

    func Run() -> int {
        val arr: ImmutableList<int> = [1, 2, 3]
        Collect(...arr)
        return arr.Count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("generic_varargs_spread_fastpath", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);
        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);

        var type = loaded.Assembly.GetType("Formatter", true)!;
        var method = type.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        Assert.NotNull(method);

        var calledMembers = ILReader.GetCalledMembers(method!);
        Assert.DoesNotContain(
            calledMembers,
            static member => member.Contains("System.Collections.Generic.List`1::Add", StringComparison.Ordinal));

        var instance = Activator.CreateInstance(type)!;
        var value = (int?)method!.Invoke(instance, Array.Empty<object>());
        Assert.Equal(3, value);
    }
}
