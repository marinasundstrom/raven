using System;
using System.IO;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class PatternLocalCodeGenTests
{
    [Fact]
    public void GuardStyleIsNotPatternLocal_IsAvailableAfterEarlyReturn()
    {
        var code = """
class GuardedText {
    func Read(value: object) -> string {
        if value is not string text {
            return "not text"
        }

        return text
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("guarded_pattern_local", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("GuardedText", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("Read")!;

        Assert.Equal("hello", method.Invoke(instance, ["hello"]));
        Assert.Equal("not text", method.Invoke(instance, [42]));
    }

    [Fact]
    public void GuardStyleIsNotPatternLocal_CanBeUsedInSubsequentComputation()
    {
        var code = """
class GuardedText {
    func ReadLength(value: object) -> int {
        if value is not string text {
            return -1
        }

        val suffix = text + "!"
        return suffix.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("guarded_pattern_local_computation", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("GuardedText", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var method = type.GetMethod("ReadLength")!;

        Assert.Equal(6, method.Invoke(instance, ["hello"]));
        Assert.Equal(-1, method.Invoke(instance, [42]));
    }
}
