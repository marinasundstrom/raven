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
        let strings: string[] = ["a", "b"]
        let objects: System.ReadOnlySpan<object> = strings
        objects.Length
    }

    static func FromSpan() -> int {
        let strings: string[] = ["a", "b", "c"]
        let mutable: System.Span<string> = strings
        let objects: System.ReadOnlySpan<object> = mutable
        objects.Length
    }

    static func FromReadOnlySpan() -> int {
        let strings: string[] = ["a", "b", "c", "d"]
        let readOnly: System.ReadOnlySpan<string> = strings
        let objects: System.ReadOnlySpan<object> = readOnly
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

    [Fact]
    public void GenericInferenceAndOverloadResolution_UseSpanConversions()
    {
        const string code = """
class SpanCalls {
    static func Length<T>(values: System.ReadOnlySpan<T>) -> int {
        values.Length
    }

    static func Choose(value: object) -> int { 1 }
    static func Choose(value: System.ReadOnlySpan<char>) -> int { 2 }

    static func ChooseSpan(value: System.Span<int>) -> int { 1 }
    static func ChooseSpan(value: System.ReadOnlySpan<int>) -> int { 2 }

    static func ChooseSequence(value: System.Collections.Generic.IEnumerable<int>) -> int { 1 }
    static func ChooseSequence(value: System.ReadOnlySpan<int>) -> int { 2 }

    static func InferredLength() -> int {
        let values: int[] = [1, 2, 3]
        Length(values)
    }

    static func PreferredOverload() -> int {
        Choose("hello")
    }

    static func PreferredSpanOverload() -> int {
        let values: int[] = [1, 2, 3]
        ChooseSpan(values)
    }

    static func PreferredSequenceOverload() -> int {
        let values: int[] = [1, 2, 3]
        ChooseSequence(values)
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation
            .Create("span_calls_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("SpanCalls", throwOnError: true)!;

        Assert.Equal(3, Invoke(type, "InferredLength"));
        Assert.Equal(2, Invoke(type, "PreferredOverload"));
        Assert.Equal(2, Invoke(type, "PreferredSpanOverload"));
        Assert.Equal(2, Invoke(type, "PreferredSequenceOverload"));
    }

    [Fact]
    public void SpanIndexingMutationSlicingAndIteration_Execute()
    {
        const string code = """
class SpanOperations {
    static func ReadAndWrite() -> int {
        let array: int[] = [10, 20, 30]
        var values: System.Span<int> = array
        values[1] = 42
        values[1]
    }

    static func ReadOnlyIndex() -> int {
        let array: int[] = [10, 20, 30]
        let values: System.ReadOnlySpan<int> = array
        values[2]
    }

    static func ReadOnlyAt(index: int) -> int {
        let array: int[] = [10, 20, 30]
        let values: System.ReadOnlySpan<int> = array
        values[index]
    }

    static func SliceLength() -> int {
        let array: int[] = [10, 20, 30, 40]
        let values: System.ReadOnlySpan<int> = array
        values.Slice(1, 2).Length
    }

    static func Sum() -> int {
        let array: int[] = [1, 2, 3, 4]
        let values: System.ReadOnlySpan<int> = array
        var sum = 0
        for value in values {
            sum += value
        }
        sum
    }

    static func SumMutable() -> int {
        let array: int[] = [2, 4, 6]
        let values: System.Span<int> = array
        var sum = 0
        for value in values {
            sum += value
        }
        sum
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation
            .Create("span_operations_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("SpanOperations", throwOnError: true)!;

        Assert.Equal(42, Invoke(type, "ReadAndWrite"));
        Assert.Equal(30, Invoke(type, "ReadOnlyIndex"));
        Assert.Equal(2, Invoke(type, "SliceLength"));
        Assert.Equal(10, Invoke(type, "Sum"));
        Assert.Equal(12, Invoke(type, "SumMutable"));

        var readOnlyAt = type.GetMethod(
            "ReadOnlyAt",
            BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        Assert.IsType<IndexOutOfRangeException>(
            Assert.Throws<TargetInvocationException>(() => readOnlyAt.Invoke(null, [-1])).InnerException);
        Assert.IsType<IndexOutOfRangeException>(
            Assert.Throws<TargetInvocationException>(() => readOnlyAt.Invoke(null, [3])).InnerException);
    }

    [Fact]
    public void CollectionExpressions_TargetSpanAndReadOnlySpan()
    {
        const string code = """
class SpanCollections {
    static func Mutable() -> int {
        let values: System.Span<int> = [10, 20, 30]
        values[1]
    }

    static func ReadOnly() -> int {
        let values: System.ReadOnlySpan<int> = [10, 20, 30, 40]
        values[3]
    }

    static func Spread() -> int {
        let middle: int[] = [20, 30]
        let values: System.ReadOnlySpan<int> = [10, ...middle, 40]
        values.Length
    }

    static func Empty() -> int {
        let values: System.Span<int> = []
        values.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation
            .Create("span_collections_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("SpanCollections", throwOnError: true)!;

        Assert.Equal(20, Invoke(type, "Mutable"));
        Assert.Equal(40, Invoke(type, "ReadOnly"));
        Assert.Equal(4, Invoke(type, "Spread"));
        Assert.Equal(0, Invoke(type, "Empty"));
    }

    [Fact]
    public void MemoryAndReadOnlyMemory_InteroperateWithSpans()
    {
        const string code = """
class MemoryOperations {
    static func Run() -> int {
        let array: int[] = [10, 20, 30]
        let memory: System.Memory<int> = array
        let readOnlyFromArray: System.ReadOnlyMemory<int> = array
        let readOnlyFromMemory: System.ReadOnlyMemory<int> = memory

        var mutableSpan = memory.Span
        mutableSpan[1] = 40

        mutableSpan[1] + readOnlyFromArray.Span[2] + readOnlyFromMemory.Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation
            .Create("memory_operations_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("MemoryOperations", throwOnError: true)!;

        Assert.Equal(73, Invoke(type, "Run"));
    }

    private static int Invoke(Type type, string methodName)
    {
        var method = type.GetMethod(
            methodName,
            BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)!;
        return (int)method.Invoke(null, Array.Empty<object>())!;
    }
}
