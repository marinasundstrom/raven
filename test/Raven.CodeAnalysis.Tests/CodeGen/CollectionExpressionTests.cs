using System.IO;
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
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var instance = Activator.CreateInstance(type!);
        var itemsCountProp = type!.GetProperty("ItemsCount");
        var emptyCountProp = type!.GetProperty("EmptyCount");

        Assert.Equal(3, (int)itemsCountProp!.GetValue(instance)!);
        Assert.Equal(0, (int)emptyCountProp!.GetValue(instance)!);
    }
}
