using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class CollectionExpressionTests
{
    [Fact]
    public void ListCollectionExpressions_AreInitializedCorrectly()
    {
        var code = """
class MyList {
    var count: int = 0
    public Add(int item) -> void { count = count + 1 }
    public Count: int { get => count }
}

class Foo {
    var items: MyList = [1, 2, 3]
    var empty: MyList = []
    public ItemsCount: int { get => items.Count }
    public EmptyCount: int { get => empty.Count }
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

        var resolver = new PathAssemblyResolver(references.Select(r => ((PortableExecutableReference)r).FilePath));
        using var mlc = new MetadataLoadContext(resolver);

        var assembly = mlc.LoadFromStream(peStream);
        var type = assembly.GetType("Foo", true);
        var instance = Activator.CreateInstance(type!);
        var itemsCountProp = type!.GetProperty("ItemsCount");
        var emptyCountProp = type!.GetProperty("EmptyCount");

        Assert.Equal(3, (int)itemsCountProp!.GetValue(instance)!);
        Assert.Equal(0, (int)emptyCountProp!.GetValue(instance)!);
    }
}
