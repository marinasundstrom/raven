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
    var data: int
    public self[index: int]: int {
        get => data;
        set => data = value;
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        var runtimePath = TargetFrameworkResolver.GetRuntimeDll(version);
        MetadataReference[] references = [
            MetadataReference.CreateFromFile(runtimePath)
        ];

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        peStream.Seek(0, SeekOrigin.Begin);
        var assembly = Assembly.Load(peStream.ToArray());
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
