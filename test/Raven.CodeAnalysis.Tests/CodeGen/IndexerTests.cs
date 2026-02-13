using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class IndexerTests
{
    [Fact]
    public void Indexer_EmitsItemPropertyWithBodies()
    {
        var code = """
class Box {
    var data: int;
    public self[index: int]: int {
        get => data;
        set => data = value;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Box", true);
        var instance = Activator.CreateInstance(type!);
        var indexer = type!.GetProperty("Item");
        Assert.NotNull(indexer);

        // Verify getter
        var initial = indexer!.GetValue(instance, new object[] { 0 });
        Assert.Equal(0, (int)initial!);

        // Verify setter
        indexer.SetValue(instance, 42, new object[] { 0 });
        var updated = indexer.GetValue(instance, new object[] { 0 });
        Assert.Equal(42, (int)updated!);
    }
}
