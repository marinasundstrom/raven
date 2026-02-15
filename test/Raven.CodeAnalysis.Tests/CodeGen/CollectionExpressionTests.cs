using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class CollectionExpressionTests
{
    [Fact]
    public void ListCollectionExpressions_AreInitializedCorrectly()
    {
        var code = """
class MyList {
    var count: int = 0
    public Add(item: int) -> unit { count = count + 1 }
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

    [Fact]
    public void ArrayCollectionExpressions_SpreadEnumerates()
    {
        var code = """
class Foo {
    public static GetCount() -> int {
        val marvel = ["Tony Stark", "Spiderman", "Thor"]
        val dc = ["Superman", "Batman", "Flash"]
        val characters = [..marvel, "Black Widow", ..dc]
        return characters.Length
    }
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
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(7, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ListCollectionExpressions_SpreadsUserDefinedItems()
    {
        var code = """
import System.Collections.Generic.*

class Item() { }

class Foo {
    public static GetCount() -> int {
        val items: List<Item> = [Item()]
        val more: List<Item> = [..items]
        return more.Count
    }
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
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(1, (int)method!.Invoke(instance, null)!);
    }
}

public class CollectionExpressionDiagnosticTests : DiagnosticTestBase
{
    [Fact]
    public void EmptyCollectionLiteral_WithoutTargetType_ReportsDiagnostic()
    {
        const string code = """
        val arr = []
        """;

        var verifier = CreateVerifier(code, [
            new DiagnosticResult("RAV2024").WithSpan(1, 11, 1, 13)
        ]);

        verifier.Verify();
    }

    [Fact]
    public void EmptyCollectionLiteral_WithTargetType_NoDiagnostic()
    {
        const string code = """
        val arr: int[] = []
        """;

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }
}
