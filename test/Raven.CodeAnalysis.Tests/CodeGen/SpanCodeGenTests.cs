using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class SpanCodeGenTests
{
    [Fact]
    public void CovariantReadOnlySpanConversions_ExposeRuntimeLength()
    {
        const string code = """
class SpanConversions {
    static func FromArray() -> int {
        val strings: string[] = ["a", "b"]
        val objects: System.ReadOnlySpan<object> = strings
        objects.Length
    }

    static func FromSpan() -> int {
        val strings: string[] = ["a", "b", "c"]
        val mutable: System.Span<string> = strings
        val objects: System.ReadOnlySpan<object> = mutable
        objects.Length
    }

    static func FromReadOnlySpan() -> int {
        val strings: string[] = ["a", "b", "c", "d"]
        val readOnly: System.ReadOnlySpan<string> = strings
        val objects: System.ReadOnlySpan<object> = readOnly
        objects.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation
            .Create("span_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("SpanConversions", throwOnError: true)!;

        Assert.Equal(2, Invoke(type, "FromArray"));
        Assert.Equal(3, Invoke(type, "FromSpan"));
        Assert.Equal(4, Invoke(type, "FromReadOnlySpan"));
    }

    private static int Invoke(Type type, string methodName)
    {
        var method = type.GetMethod(
            methodName,
            BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        return (int)method.Invoke(null, Array.Empty<object>())!;
    }
}
