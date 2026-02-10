using Raven.CodeAnalysis.Symbols;

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
}
