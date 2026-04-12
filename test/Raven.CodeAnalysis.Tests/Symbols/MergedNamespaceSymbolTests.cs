using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class MergedNamespaceSymbolTests
{
    [Fact]
    public void Constructor_WithEmptyNamespaces_ThrowsArgumentException()
    {
        var ex = Assert.Throws<ArgumentException>(() => new MergedNamespaceSymbol([], null!));
        Assert.Equal("namespaces", ex.ParamName);
    }

    [Fact]
    public void Constructor_WithSingleNamespace_PreservesNamespaceName()
    {
        var compilation = Compilation.Create(
            "test",
            [],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var merged = new MergedNamespaceSymbol([compilation.GlobalNamespace], null!);

        Assert.Equal(compilation.GlobalNamespace.Name, merged.Name);
    }

    [Fact]
    public void Constructor_WithNullNamespaceEntries_IgnoresNulls()
    {
        var compilation = Compilation.Create(
            "test",
            [],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var merged = new MergedNamespaceSymbol([null!, compilation.GlobalNamespace], null!);

        Assert.Equal(compilation.GlobalNamespace.Name, merged.Name);
    }

    [Fact]
    public async Task GetSemanticModel_WhenSetupRunsConcurrently_DoesNotObserveHalfInitializedGlobalNamespace()
    {
        var tree = SyntaxTree.ParseText(
            """
            import System.*

            func Main() -> () {
                val value = 42
            }
            """,
            path: "/tmp/test.rav");

        var compilation = Compilation.Create(
            "test",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var tasks = Enumerable.Range(0, 8)
            .Select(_ => Task.Run(() => compilation.GetSemanticModel(tree)))
            .ToArray();

        await Task.WhenAll(tasks);

        foreach (var task in tasks)
            Assert.NotNull(task.Result);
    }
}
