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
    public var self[index: int]: int {
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

    [Fact]
    public void Indexer_ExpressionBodies_CanAccessFieldOnlyStoredPropertyElement()
    {
        var code = """
class Box {
    private var data: int[] = [1, 2, 3]

    public var self[index: int]: int {
        get => data[index]
        set => data[index] = value
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

        var initial = indexer!.GetValue(instance, new object[] { 1 });
        Assert.Equal(2, (int)initial!);

        indexer.SetValue(instance, 42, new object[] { 1 });
        var updated = indexer.GetValue(instance, new object[] { 1 });
        Assert.Equal(42, (int)updated!);
    }

    [Fact]
    public void Indexer_OnStructReceiver_EmitsAndRuns()
    {
        var code = """
struct Counter {
    private var data: int[] = [10, 20, 30]

    public var self[index: int]: int {
        get => data[index]
        set => data[index] = value
    }
}

class Harness {
    public static func Run() -> int {
        var counter = Counter()
        counter[1] = 42
        return counter[1]
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
        var type = assembly.GetType("Harness", true);
        var method = type!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(42, (int)method!.Invoke(null, null)!);
    }

    [Fact]
    public void Indexer_OnStructReturnedFromMethod_EmitsAndRuns()
    {
        var code = """
struct Counter {
    private var data: int[] = [10, 20, 30]

    public var self[index: int]: int {
        get => data[index]
    }
}

class Holder {
    public func GetCounter() -> Counter {
        Counter()
    }
}

class Harness {
    public static func Run() -> int {
        return Holder().GetCounter()[2]
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
        var type = assembly.GetType("Harness", true);
        var method = type!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(30, (int)method!.Invoke(null, null)!);
    }
}
