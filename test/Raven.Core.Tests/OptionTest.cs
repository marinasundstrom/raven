using System.Reflection;
using System.Text.Json;

using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class OptionTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_Some()
    {
        var asm = LoadRavenCoreAssembly();
        var optionType = GetConstructedType(asm, "System.Option`1", typeof(int));
        var someType = GetConstructedNestedType(optionType, "Some");
        var some = Activator.CreateInstance(someType, 42)!;
        var option = ConvertCaseToCarrier(optionType, some);

        var json = JsonSerializer.Serialize(option, optionType);
        var parsed = JsonSerializer.Deserialize(json, optionType);

        Assert.NotNull(parsed);

        var tryGetSome = optionType.GetMethod("TryGetSome")!;
        var args = new object?[] { null };
        var ok = (bool)(tryGetSome.Invoke(parsed, args) ?? false);
        Assert.True(ok);
        Assert.NotNull(args[0]);

        var value = args[0]!.GetType().GetProperty("Value")!.GetValue(args[0]);
        Assert.Equal(42, value);
    }

    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_None()
    {
        var asm = LoadRavenCoreAssembly();
        var optionType = GetConstructedType(asm, "System.Option`1", typeof(int));
        var noneType = GetConstructedNestedType(optionType, "None");
        var none = Activator.CreateInstance(noneType)!;
        var option = ConvertCaseToCarrier(optionType, none);

        var json = JsonSerializer.Serialize(option, optionType);
        var parsed = JsonSerializer.Deserialize(json, optionType);

        Assert.NotNull(parsed);

        var tryGetNone = optionType.GetMethod("TryGetNone")!;
        var args = new object?[] { null };
        var ok = (bool)(tryGetNone.Invoke(parsed, args) ?? false);
        Assert.True(ok);
    }

    [Fact]
    public void MapThenWhere_BindsFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val input: Option<int> = .Some(20)
val mapped = input.Map(v => v + 1)
val filtered = mapped.Where(v => v > 10)
val output = filtered.UnwrapOr(0)
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void Map_WithWrongLambdaType_ProducesDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

val input: Option<int> = .Some(20)
val _ = input.Map((v: string) => v)
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501")
                    .WithAnySpan()
                    .WithArguments("method", "Map", 1)
            ]);

        verifier.Verify();
    }

    private static Assembly LoadRavenCoreAssembly()
    {
        var path = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        return Assembly.LoadFrom(path);
    }

    private static Type GetConstructedType(Assembly assembly, string metadataName, params Type[] typeArgs)
    {
        var definition = assembly.GetType(metadataName, throwOnError: true)!;
        return definition.MakeGenericType(typeArgs);
    }

    private static Type GetConstructedNestedType(Type constructedOuter, string nestedName)
    {
        var nested = constructedOuter.GetNestedType(nestedName, BindingFlags.Public | BindingFlags.NonPublic)
            ?? throw new InvalidOperationException($"Missing nested type '{nestedName}' on '{constructedOuter}'.");

        if (nested.IsGenericTypeDefinition && constructedOuter.IsGenericType)
            nested = nested.MakeGenericType(constructedOuter.GetGenericArguments());

        return nested;
    }

    private static object ConvertCaseToCarrier(Type carrierType, object caseValue)
    {
        var conversion = carrierType.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)
            .FirstOrDefault(m =>
                m.Name == "op_Implicit" &&
                m.ReturnType == carrierType &&
                m.GetParameters().Length == 1 &&
                m.GetParameters()[0].ParameterType == caseValue.GetType());

        if (conversion is null)
            throw new InvalidOperationException($"Missing implicit conversion from '{caseValue.GetType()}' to '{carrierType}'.");

        return conversion.Invoke(null, [caseValue])!;
    }
}
