using System;
using System.Linq;
using System.Reflection;
using System.Text.Json;

using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class ResultTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_Ok()
    {
        var asm = LoadRavenCoreAssembly();
        var resultType = GetConstructedType(asm, "System.Result`2", typeof(int), typeof(string));
        var okType = GetCaseTypeFromTryGetValue(resultType, "Ok");
        var okCase = Activator.CreateInstance(okType, 123)!;
        var result = ConvertCaseToCarrier(resultType, okCase);

        var json = JsonSerializer.Serialize(result, resultType);
        var parsed = JsonSerializer.Deserialize(json, resultType);

        Assert.NotNull(parsed);

        var tryGetOk = GetTryGetValueMethod(resultType, okType);
        var args = new object?[] { null };
        var ok = (bool)(tryGetOk.Invoke(parsed, args) ?? false);
        Assert.True(ok);
        Assert.NotNull(args[0]);

        var value = args[0]!.GetType().GetProperty("Value")!.GetValue(args[0]);
        Assert.Equal(123, value);
    }

    [Fact]
    public void JsonSerializer_SerializesAndDeserializes_Error()
    {
        var asm = LoadRavenCoreAssembly();
        var resultType = GetConstructedType(asm, "System.Result`2", typeof(int), typeof(string));
        var errorType = GetCaseTypeFromTryGetValue(resultType, "Error");
        var errorCase = Activator.CreateInstance(errorType, "boom")!;
        var result = ConvertCaseToCarrier(resultType, errorCase);

        var json = JsonSerializer.Serialize(result, resultType);
        var parsed = JsonSerializer.Deserialize(json, resultType);

        Assert.NotNull(parsed);

        var tryGetError = GetTryGetValueMethod(resultType, errorType);
        var args = new object?[] { null };
        var ok = (bool)(tryGetError.Invoke(parsed, args) ?? false);
        Assert.True(ok);
        Assert.NotNull(args[0]);

        var data = args[0]!.GetType().GetProperty("Data")!.GetValue(args[0]);
        Assert.Equal("boom", data);
    }

    [Fact]
    public void MapErrorAndUnwrapError_BindFromRavenCore()
    {
        const string code = """
import System.*
import System.Linq.*

val result: Result<int, string> = .Error("boom")
val hasError = result.HasError
val text = result.MapError(message => message + "!").UnwrapError()
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void MapError_WithWrongLambdaType_ProducesDiagnostic()
    {
        const string code = """
import System.*
import System.Linq.*

val result: Result<int, string> = .Error("boom")
val _ = result.MapError((v: int) => v)
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1501")
                    .WithAnySpan()
                    .WithArguments("method", "MapError", 1)
            ]);

        verifier.Verify();
    }

    [Fact]
    public void MapError_InGenericExtensionMethod_BindsWithoutNestedTypeInferenceArtifacts()
    {
        const string code = """
import System.*
import System.Linq.*
import System.Collections.Generic.*

extension TestExt<T> for IEnumerable<T> {
    ToArrayOrError2<E>(errorFactory: Exception -> E) -> Result<T[], E> {
        val result: Result<T[], Exception> = .Ok([])
        result.MapError(errorFactory)
    }
}
""";

        CreateVerifier(code).Verify();
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

    private static MethodInfo GetTryGetValueMethod(Type resultType, Type caseType)
    {
        return resultType.GetMethod(
            "TryGetValue",
            BindingFlags.Public | BindingFlags.Instance,
            binder: null,
            types: [caseType.MakeByRefType()],
            modifiers: null)
            ?? throw new InvalidOperationException($"Missing TryGetValue(out {caseType}) on '{resultType}'.");
    }

    private static Type GetCaseTypeFromTryGetValue(Type resultType, string caseName)
    {
        var caseType = resultType
            .GetMethods(BindingFlags.Public | BindingFlags.Instance)
            .Where(method => method.Name == "TryGetValue" && method.ReturnType == typeof(bool))
            .SelectMany(method => method.GetParameters())
            .Where(parameter => parameter.ParameterType.IsByRef)
            .Select(parameter => parameter.ParameterType.GetElementType())
            .FirstOrDefault(type => type is not null && type.Name.Contains(caseName, StringComparison.Ordinal));

        return caseType
            ?? throw new InvalidOperationException($"Missing TryGetValue overload for case '{caseName}' on '{resultType}'.");
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
