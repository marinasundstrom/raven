using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text.Json;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class CollectionExpressionTests
{
    [Fact]
    public void DictionaryExpression_InConstructorArgument_UsesDictionaryParameterTargetType()
    {
        var code = """
import System.Collections.Generic.*

union JsonValue(string | double | bool | JsonObject)
record JsonObject(Properties: IDictionary<string, JsonValue>)

class Foo {
    public static func GetCount() -> int {
        val value = JsonObject([
            "name": 42
        ])
        return value.Properties.Count
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
        var method = type!.GetMethod("GetCount", BindingFlags.Static | BindingFlags.Public);

        Assert.Equal(1, (int)method!.Invoke(null, null)!);
    }

    [Fact]
    public void DictionaryExpression_UnionValueArrayBranch_TargetsNestedCollectionExpression()
    {
        var code = """
import System.Collections.Generic.*

union JsonValue(string | double | bool | JsonObject | JsonValue[])
record JsonObject(Properties: IDictionary<string, JsonValue>)

class Foo {
    public static func GetCount() -> int {
        val value = JsonObject([
            "name": 42,
            "items": [ "true", 42 ]
        ])
        return value.Properties.Count
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
        var method = type!.GetMethod("GetCount", BindingFlags.Static | BindingFlags.Public);

        Assert.Equal(2, (int)method!.Invoke(null, null)!);
    }

    [Fact]
    public void DictionaryExpression_UnionValueArrayBranch_SerializesWithParenthesizedUnionConverter()
    {
        var code = """
import System.*
import System.Collections.Generic.*
import System.Text.Json.*
import System.Text.Json.Serialization.*

[JsonConverter(typeof(RavenParenthesizedUnionJsonConverterFactory))]
union JsonValue(string | double | bool | JsonObject | JsonValue[])

[JsonConverter(typeof(JsonObjectConverter))]
record JsonObject(Properties: IDictionary<string, JsonValue>)

class JsonObjectConverter : JsonConverter<JsonObject> {
    override func Read(
        reader: &Utf8JsonReader,
        typeToConvert: Type,
        options: JsonSerializerOptions
    ) -> JsonObject {
        use doc = JsonDocument.ParseValue(reader)
        val root = doc.RootElement

        if root.ValueKind != JsonValueKind.Object {
            throw JsonException("JsonObject must be a JSON object.")
        }

        val properties = Dictionary<string, JsonValue>()

        for property in root.EnumerateObject() {
            properties.Add(property.Name, ReadJsonValue(property.Value, options))
        }

        return JsonObject(properties)
    }

    override func Write(
        writer: Utf8JsonWriter,
        value: JsonObject,
        options: JsonSerializerOptions
    ) -> () {
        writer.WriteStartObject()

        for property in value.Properties {
            writer.WritePropertyName(property.Key)
            JsonSerializer.Serialize(writer, property.Value, options)
        }

        writer.WriteEndObject()
    }

    private static func ReadJsonValue(element: JsonElement, options: JsonSerializerOptions) -> JsonValue {
        if element.ValueKind == JsonValueKind.String {
            return JsonValue(element.GetString() ?? "")
        }

        if element.ValueKind == JsonValueKind.Number {
            return JsonValue(element.GetDouble())
        }

        if element.ValueKind == JsonValueKind.True {
            return JsonValue(true)
        }

        if element.ValueKind == JsonValueKind.False {
            return JsonValue(false)
        }

        if element.ValueKind == JsonValueKind.Array {
            val values = List<JsonValue>()

            for item in element.EnumerateArray() {
                values.Add(ReadJsonValue(item, options))
            }

            return JsonValue(values.ToArray())
        }

        if element.ValueKind == JsonValueKind.Object {
            val properties = Dictionary<string, JsonValue>()

            for property in element.EnumerateObject() {
                properties.Add(property.Name, ReadJsonValue(property.Value, options))
            }

            return JsonValue(JsonObject(properties))
        }

        throw JsonException("Unsupported JSON value kind '$element.ValueKind'.")
    }
}

class Foo {
    public static func Serialize() -> string {
        val options = JsonSerializerOptions {
            PropertyNamingPolicy = .CamelCase
        }

        val value = JsonObject([
            "name": 32,
            "items": [ "true", 42 ]
        ])

        return JsonSerializer.Serialize(value, options)
    }

    public static func RoundTripCount() -> int {
        val options = JsonSerializerOptions {
            PropertyNamingPolicy = .CamelCase
        }

        val value = JsonSerializer.Deserialize<JsonObject>(Serialize(), options)
        return value.Properties.Count
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default
            .Concat([MetadataReference.CreateFromFile(GetRavenCorePath())])
            .ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        CollectionExpressionTestHelpers.AssertSuccess(result);

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("Serialize", BindingFlags.Static | BindingFlags.Public);
        var json = (string)method!.Invoke(null, null)!;
        var roundTripMethod = type.GetMethod("RoundTripCount", BindingFlags.Static | BindingFlags.Public);

        using var document = JsonDocument.Parse(json);

        var name = document.RootElement.GetProperty("name");
        Assert.Equal(JsonValueKind.Number, name.ValueKind);
        Assert.Equal(32, name.GetDouble());

        var items = document.RootElement.GetProperty("items");
        Assert.Equal(JsonValueKind.Array, items.ValueKind);
        Assert.Equal(2, items.GetArrayLength());
        Assert.Equal("true", items[0].GetString());
        Assert.Equal(42, items[1].GetDouble());
        Assert.Equal(2, (int)roundTripMethod!.Invoke(null, null)!);
    }

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
    public void CollectionExpressions_SpreadEnumerates()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val marvel = ["Tony Stark", "Spiderman", "Thor"]
        val dc = ["Superman", "Batman", "Flash"]
        val characters = [...marvel, "Black Widow", ...dc]
        return characters.Count
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
        val more: Item[] = [...items]
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
    public void InferredImmutableListCollectionExpressions_AreInitializedCorrectly()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val values = [1, 2, 3]
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
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(3, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void DictionaryCollectionExpressions_AreInitializedCorrectly()
    {
        var code = """
import System.Collections.Immutable.*
import System.Collections.Generic.*

class Foo {
    static func GetCount() -> int {
        val values = ["a": 1, "bb": 2]
        return values.Count + values["bb"]
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

        Assert.Equal(4, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void MutableDictionaryCollectionExpressions_UseDictionaryFallback()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func GetCount() -> int {
        val values = !["a": 1, "bb": 2]
        return values.Count + values["bb"]
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

        Assert.Equal(4, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void DictionaryCollectionExpressions_SupportSpreadEntriesAndDictionarySpreads()
    {
        var code = """
import System.Collections.Immutable.*
import System.Collections.Generic.*

class Foo {
    static func GetCount() -> int {
        val other: Dictionary<string, int> = !["bb": 2]
        val values = [..."a": 1, ...other, "ccc": 3]
        return values.Count + values["bb"]
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

        Assert.Equal(5, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void DictionaryCollectionExpressions_SupportDictionaryComprehension()
    {
        var code = """
import System.Collections.Immutable.*

class Foo {
    static func GetCount() -> int {
        val values = [for key in [|"a", "bb"|] => key: key.Length]
        return values.Count + values["bb"]
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

        Assert.Equal(4, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void DictionaryCollectionExpressions_CanTargetDictionaryInterfaces()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func GetCount() -> int {
        val values: IReadOnlyDictionary<string, int> = ["a": 1, "bb": 2]
        return values.Count + values["bb"]
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

        Assert.Equal(4, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void InferredImmutableListCollectionExpressions_WithSourceDefinedElement_UseImmutableListFactory()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val values = [Person("Jane"), Person("Bob")]
        return values.Count
    }
}

record Person(Name: string)
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
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.Contains(
            calledMembers,
            static member => member.Contains("System.Collections.Immutable.ImmutableList::CreateRange", StringComparison.Ordinal));
        Assert.Equal(2, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void InferredMutableListCollectionExpressions_AreInitializedCorrectly()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val values = ![1, 2, 3]
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
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(3, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void InferredArrayCollectionExpressions_AreInitializedCorrectly()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val values = [|1, 2, 3|]
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
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(3, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void InferredArrayCollectionExpressions_WithSpread_AreInitializedCorrectly()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val prefix = [|1, 2|]
        val values = [|...prefix, 3|]
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
        var assembly = loaded.Assembly;
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetCount");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(3, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ExpressionBodiedMethod_ImmutableArrayTarget_UsesTargetTypedCollectionEmission()
    {
        var code = """
import System.Collections.Immutable.*

class Foo {
    static func Create() -> ImmutableArray<int> => [1, 2, 3]
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
        var method = type!.GetMethod("Create", BindingFlags.Public | BindingFlags.Static);

        var values = (ImmutableArray<int>)method!.Invoke(null, null)!;

        Assert.Equal(3, values.Length);
        Assert.Equal([1, 2, 3], values.ToArray());
    }

    [Fact]
    public void ArrayCollectionExpressions_ArraySpread_UsesArrayCopyFastPath()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val left: int[] = [1, 2]
        val right: int[] = [3, 4]
        val values: int[] = [...left, 9, ...right]
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
    public void CollectionExpressions_SupportDeconstructionComprehension()
    {
        var code = """
import System.Collections.Immutable.*

class Foo {
    static func GetNames() -> string {
        val people = [(1, "Ada"), (2, "Bob"), (1, "Ignored")]
        val names = [for val (2, name) in people => name]
        return names[0]
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        stream.Position = 0;
        var assembly = Assembly.Load(stream.ToArray());
        var type = assembly.GetType("Foo", true);
        var method = type!.GetMethod("GetNames");

        Assert.Equal("Bob", (string)method!.Invoke(null, null)!);
    }

    [Fact]
    public void EnumerableCollectionExpressions_ArraySpread_UsesArrayCopyFastPath()
    {
        var code = """
import System.Collections.Generic.*
import System.Linq.*

class Foo {
    static func GetCount() -> int {
        val left: int[] = [1, 2]
        val right: int[] = [3]
        val values: IEnumerable<int> = [...left, 9, ...right]
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
        val values: int[] = [...source]
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
import System.Linq.*

class Foo {
    static func GetCount() -> int {
        val source: int[] = [1, 2, 3]
        val values: IEnumerable<int> = [...source]
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
        val merged: char[] = ['x', ..."ab", 'y']
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
    public void CollectionDeconstruction_WithoutRest_UsesDirectIndexAccess()
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
            static member => member.Contains("System.Collections.IEnumerable::GetEnumerator", StringComparison.Ordinal));
        Assert.Contains(
            calledMembers,
            static member => member.Contains("::get_Item", StringComparison.Ordinal));
        Assert.Equal(5, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void DictionaryDeconstruction_AssignsValuesByKey()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func GetPairSum() -> int {
        val values: IReadOnlyDictionary<string, int> = ["a": 2, "b": 3]
        val ["a": first, "b": second] = values
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
        var method = type!.GetMethod("GetPairSum");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.Contains(
            calledMembers,
            static member =>
                member.Contains("IReadOnlyDictionary", StringComparison.Ordinal) &&
                member.Contains("::get_Item", StringComparison.Ordinal));
        Assert.Equal(5, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void DictionaryPattern_MatchExpression_UsesTryGetValue()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func GetValue() -> int {
        val values: Dictionary<string, int> = !["a": 5, "b": 2]
        return match values {
            ["a": val first, "b": 2] => first
            _ => 0
        }
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
        var method = type!.GetMethod("GetValue");
        var calledMembers = ILReader.GetCalledMembers(method!);
        var instance = Activator.CreateInstance(type!);

        Assert.Contains(
            calledMembers,
            static member => member.Contains("TryGetValue", StringComparison.Ordinal));
        Assert.Equal(5, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void TuplePattern_WithNestedDictionaryAndSequencePatterns_Matches()
    {
        var code = """
import System.Collections.Generic.*

class Foo {
    static func Route() -> string {
        val headers: IReadOnlyDictionary<string, string> = ["event": "scan"]
        val segments: string[] = ["parcels", "P-1"]

        return (headers, segments) match {
            (["event": "scan"], ["parcels", val id]) => id
            _ => "no match"
        }
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
        var method = type!.GetMethod("Route");
        var instance = Activator.CreateInstance(type!);

        Assert.Equal("P-1", (string)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void SpreadInference_PreservesImmutableCollectionBuilderType()
    {
        var code = """
import System.Collections.Immutable.*

class Foo {
    static func Accept(values: ImmutableList<int>) -> int {
        return values.Count
    }

    static func GetCount() -> int {
        val list: ImmutableList<int> = [2, 3, 4]
        val newList = [7, ...list, 5]
        return Accept(newList)
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
            static member => member.Contains("System.Collections.Immutable.ImmutableList::Create", StringComparison.Ordinal) ||
                             member.Contains("System.Collections.Immutable.ImmutableList::CreateRange", StringComparison.Ordinal));
        Assert.Equal(5, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ArrayCollectionExpressions_RangeElement_ExpandsSequence()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val values: int[] = [1..3]
        return values.Length + values[0] + values[2]
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
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(7, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ArrayCollectionExpressions_RangeElement_CanBeMixedWithOrdinaryElements()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val values: int[] = [1, 3..4, 9]
        return values.Length + values[1] + values[2]
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
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(11, (int)method!.Invoke(instance, null)!);
    }

    [Fact]
    public void ArrayCollectionExpressions_ExclusiveRangeElement_StopsBeforeUpperBound()
    {
        var code = """
class Foo {
    static func GetCount() -> int {
        val values: int[] = [1..<4]
        return values.Length + values[0] + values[2]
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
        var instance = Activator.CreateInstance(type!);

        Assert.Equal(7, (int)method!.Invoke(instance, null)!);
    }

    private static string GetRavenCorePath()
    {
        var outputPath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (File.Exists(outputPath))
            return outputPath;

        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        return Path.Combine(repoRoot, "src", "Raven.Core", "bin", "Debug", "net10.0", "Raven.Core.dll");
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
