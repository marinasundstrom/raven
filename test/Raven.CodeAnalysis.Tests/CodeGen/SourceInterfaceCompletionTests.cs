using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SourceInterfaceCompletionTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false)]
    [InlineData(true)]
    public void ExplicitInterfaceIndexerDispatchesThroughInterface(bool expressionBody)
    {
        var contract = expressionBody
            ? "val self[index: int]: int { get; }"
            : "var self[index: int]: int { get; set; }";
        var implementation = expressionBody
            ? "val Indexed.self[index: int]: int => index + 40"
            : "var Indexed.self[index: int]: int { get => stored + index; set { stored = value - index } }";
        var assignment = expressionBody ? "" : "buffer[2] = 42";
        var source = $$"""
interface Indexed { {{contract}} }
class Buffer : Indexed {
    private field stored: int = 0

    {{implementation}}
}
func Main() -> int {
    let buffer: Indexed = Buffer()
    {{assignment}}
    return buffer[2]
}
""";
        var compilation = Compilation.Create("ExplicitIndexerDispatch" + expressionBody, [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var buffer = compilation.GetTypeByMetadataName("Buffer")!;
        var indexer = Assert.Single(buffer.GetMembers().OfType<PropertySymbol>());
        Assert.True(indexer.IsIndexer);
        Assert.Single(indexer.ExplicitInterfaceImplementations);
        Assert.Single(indexer.GetMethod!.ExplicitInterfaceImplementations);
        if (!expressionBody)
            Assert.Single(indexer.SetMethod!.ExplicitInterfaceImplementations);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void CrossFileInterfaceClosureAndIndexerRemainAvailable(bool reverse)
    {
        string[] sources = [
            "public interface Root<T> { val Count: int { get } }",
            "public interface Middle<T> : Root<T> { }",
            "public interface Indexed<T> : Middle<T> { val self[index: int]: T { get } }",
            """
            public interface Closed : Indexed<int> { }
            public class Buffer : Closed {
                val Count: int => 40
                val self[index: int]: int => index + 2
            }
            func Main() -> int {
                let buffer: Closed = Buffer()
                return buffer.Count + buffer[0]
            }
            """
        ];
        var trees = sources.Select((source, i) => SyntaxTree.ParseText(source, path: $"Part{i}.rvn")).ToArray();
        if (reverse) Array.Reverse(trees);
        var compilation = Compilation.Create("InterfaceCompletion" + reverse, trees, TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var closed = (NamedTypeSymbol)compilation.GetTypeByMetadataName("Closed")!;
        Assert.Equal(3, closed.AllInterfaces.Length);
        Assert.All(closed.AllInterfaces, i => Assert.Equal(SpecialType.System_Int32, Assert.Single(i.TypeArguments).SpecialType));
        var indexed = closed.AllInterfaces.Single(i => i.Name == "Indexed");
        var indexer = Assert.Single(indexed.GetMembers().OfType<PropertySymbol>());
        Assert.True(indexer.IsIndexer);
        Assert.True(indexer.GetMethod!.IsAbstract);
        Assert.Equal(SpecialType.System_Int32, indexer.Type.SpecialType);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var loaded = Assembly.Load(image.ToArray());
        Assert.Equal(42, loaded.EntryPoint!.Invoke(null, null));
    }
}
