using System.IO;
using System.Linq;
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
    func Add(item: int) -> unit { count = count + 1 }
    val Count: int {
        get => count
    }
}

class Foo {
    var items: MyList = [1, 2, 3]
    var empty: MyList = []
    val ItemsCount: int {
        get => items.Count
    }
    val EmptyCount: int {
        get => empty.Count
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

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
    static func GetCount() -> int {
        val marvel = ["Tony Stark", "Spiderman", "Thor"]
        val dc = ["Superman", "Batman", "Flash"]
        val characters = [..marvel, "Black Widow", ..dc]
        return characters.Length
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(7, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ArrayCollectionExpressions_SpreadsUserDefinedItems()
    {
        var code = """
class Item() { }

class Foo {
    static func GetCount() -> int {
        val items: Item[] = [Item()]
        val more: Item[] = [..items]
        return more.Length
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(1, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ArrayCollectionExpressions_ArraySpread_UsesArrayCopyFastPath()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val left: int[] = [1, 2]
        val right: int[] = [3, 4]
        val values: int[] = [..left, 9, ..right]
        return values.Length
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(2, calledMembers.Count(static member => member == "System.Array::Copy"));
        Assert.DoesNotContain(
            calledMembers,
            static member => member.Contains("System.Collections.Generic.List`1::Add", StringComparison.Ordinal));
        Assert.Equal(5, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void EnumerableCollectionExpressions_ArraySpread_UsesArrayCopyFastPath()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func GetCount() -> int {
        val left: int[] = [1, 2]
        val right: int[] = [3]
        val values: IEnumerable<int> = [..left, 9, ..right]
        return values.Count()
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(2, calledMembers.Count(static member => member == "System.Array::Copy"));
        Assert.DoesNotContain(
            calledMembers,
            static member => member.Contains("System.Collections.Generic.List`1::Add", StringComparison.Ordinal));
        Assert.Equal(4, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ArrayCollectionExpressions_SingleArraySpread_ReusesSourceArray()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val source: int[] = [1, 2, 3]
        val values: int[] = [..source]
        return values.Length
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.DoesNotContain(calledMembers, static member => member == "System.Array::Copy");
        Assert.DoesNotContain(
            calledMembers,
            static member => member.Contains("System.Collections.Generic.List`1::Add", StringComparison.Ordinal));
        Assert.Equal(3, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void EnumerableCollectionExpressions_SingleArraySpread_ReusesSourceArray()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func GetCount() -> int {
        val source: int[] = [1, 2, 3]
        val values: IEnumerable<int> = [..source]
        return values.Count()
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.DoesNotContain(calledMembers, static member => member == "System.Array::Copy");
        Assert.DoesNotContain(
            calledMembers,
            static member => member.Contains("System.Collections.Generic.List`1::Add", StringComparison.Ordinal));
        Assert.Equal(3, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ArrayCollectionExpressions_NonArraySpread_UsesListFallback()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func GetCount() -> int {
        val merged: char[] = ['x', .."ab", 'y']
        return merged.Length
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.DoesNotContain(calledMembers, static member => member == "System.Array::Copy");
        Assert.Contains(
            calledMembers,
            static member => member.Contains("System.Collections.IEnumerable::GetEnumerator", StringComparison.Ordinal));
        Assert.Equal(4, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void CollectionBuilderAttribute_Target_UsesBuilderFactoryMethod()
    {
        var code = """
import System.Collections.Immutable.*

class Foo {
    static func GetCount() -> int {
        val values: ImmutableList<int> = [2, 3, 4]
        return values.Count
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.Contains(
            calledMembers,
            static member => member.Contains("System.Collections.Immutable.ImmutableList::Create", StringComparison.Ordinal));
        Assert.DoesNotContain(
            calledMembers,
            static member => member.Contains("System.Collections.Generic.List`1::Add", StringComparison.Ordinal));
        Assert.Equal(3, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void CollectionDeconstruction_WithoutRest_IndexesDirectly()
    {
        var code = """
import System.Collections.Immutable.*

class Foo {
    static func GetFirstPairSum() -> int {
        val values: ImmutableList<int> = [2, 3, 4]
        val [first, second] = values
        return first + second
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
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetFirstPairSum");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.DoesNotContain(
            calledMembers,
            static member => member.Contains("::get_Count", StringComparison.Ordinal));
        Assert.Equal(
            2,
            calledMembers.Count(static member => member.Contains("::get_Item", StringComparison.Ordinal)));
        Assert.Equal(5, (int)method!.Invoke(instance, null)!);
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

file static class CollectionExpressionTestHelpers
{
    public static void AssertSuccess(EmitResult result)
    {
        if (result.Success)
            return;

        var diagnostics = string.Join(
            Environment.NewLine,
            result.Diagnostics.Select(static d => d.ToString()));

        Assert.True(result.Success, diagnostics);
    }
}
