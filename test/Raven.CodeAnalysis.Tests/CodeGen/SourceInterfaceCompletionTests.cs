using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class SourceInterfaceCompletionTests
{
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
        var closed = (INamedTypeSymbol)compilation.GetTypeByMetadataName("Closed")!;
        Assert.Equal(3, closed.AllInterfaces.Length);
        Assert.All(closed.AllInterfaces, i => Assert.Equal(SpecialType.System_Int32, Assert.Single(i.TypeArguments).SpecialType));
        var indexed = closed.AllInterfaces.Single(i => i.Name == "Indexed");
        var indexer = Assert.Single(indexed.GetMembers().OfType<IPropertySymbol>());
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
