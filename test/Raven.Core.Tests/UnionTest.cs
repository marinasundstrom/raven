using System.Reflection;
using System.Text.Json;

namespace Raven.Core.Tests;

public sealed class UnionTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void StandardUnion_IsStructCarrier_WithDefaultUninitializedState()
    {
        var asm = LoadRavenCoreAssembly();
        var unionType = GetConstructedType(asm, "System.Union`2", typeof(string), typeof(int));
        var defaultUnion = Activator.CreateInstance(unionType)!;

        Assert.True(unionType.IsValueType);
        Assert.Equal(false, unionType.GetProperty("HasValue")!.GetValue(defaultUnion));
        Assert.Null(unionType.GetProperty("Value")!.GetValue(defaultUnion));
    }

    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_FirstMemberType()
    {
        var asm = LoadRavenCoreAssembly();
        var unionType = GetConstructedType(asm, "System.Union`2", typeof(string), typeof(int));
        var union = CreateUnion(unionType, "hello");

        var json = JsonSerializer.Serialize(union, unionType);

        Assert.Equal("\"hello\"", json);

        var parsed = JsonSerializer.Deserialize(json, unionType);

        Assert.NotNull(parsed);
        Assert.Equal(true, unionType.GetProperty("HasValue")!.GetValue(parsed));
        Assert.Equal("hello", unionType.GetProperty("Value")!.GetValue(parsed));
    }

    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_SecondPrimitiveMemberType()
    {
        var asm = LoadRavenCoreAssembly();
        var unionType = GetConstructedType(asm, "System.Union`2", typeof(string), typeof(int));
        var union = CreateUnion(unionType, 42);

        var json = JsonSerializer.Serialize(union, unionType);

        Assert.Equal("42", json);

        var parsed = JsonSerializer.Deserialize(json, unionType);

        Assert.NotNull(parsed);
        Assert.Equal(true, unionType.GetProperty("HasValue")!.GetValue(parsed));
        Assert.Equal(42, unionType.GetProperty("Value")!.GetValue(parsed));
    }

    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_ComplexMemberType()
    {
        var asm = LoadRavenCoreAssembly();
        var unionType = GetConstructedType(asm, "System.Union`2", typeof(Car), typeof(string));
        var union = CreateUnion(unionType, new Car("Volvo"));

        var json = JsonSerializer.Serialize(union, unionType);

        Assert.Equal("""{"$type":"Car","Make":"Volvo"}""", json);

        var parsed = JsonSerializer.Deserialize(json, unionType);

        Assert.NotNull(parsed);
        Assert.Equal(true, unionType.GetProperty("HasValue")!.GetValue(parsed));
        Assert.Equal(new Car("Volvo"), unionType.GetProperty("Value")!.GetValue(parsed));
    }

    [Fact]
    public void JsonSerializer_SerializesPrimitiveUnionProperty_AsPrimitivePropertyValue()
    {
        var asm = LoadRavenCoreAssembly();
        var unionType = GetConstructedType(asm, "System.Union`2", typeof(string), typeof(int));
        var objType = typeof(Obj<>).MakeGenericType(unionType);
        var union = CreateUnion(unionType, "str");
        var obj = Activator.CreateInstance(objType, union)!;
        var options = new JsonSerializerOptions(JsonSerializerDefaults.Web);

        var json = JsonSerializer.Serialize(obj, objType, options);

        Assert.Equal("""{"value":"str"}""", json);
    }

    private static Type GetConstructedType(Assembly assembly, string metadataName, params Type[] typeArgs)
    {
        var definition = assembly.GetType(metadataName, throwOnError: true)!;
        return definition.MakeGenericType(typeArgs);
    }

    private static object CreateUnion(Type unionType, object memberValue)
    {
        var constructor =
            unionType.GetConstructor([memberValue.GetType()])
                ?? throw new InvalidOperationException($"Missing constructor from '{memberValue.GetType()}' to '{unionType}'.");

        return constructor.Invoke([memberValue]);
    }

    private sealed record Car(string Make);

    private sealed record Obj<T>(T Value);
}
