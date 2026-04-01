using System;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class ParseExtensionsTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void IntParse_BindsFromRavenCore_AndSupportsResultPropagation()
    {
        const string code = """
import System.*

func ParseMain(text: string) -> Result<int, IParseError> {
    val value = int.parse(text)?
    return Ok(value)
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void IntParse_ReturnsOk_ForValidNumber()
    {
        var assembly = LoadRavenCoreAssembly();
        var extensions = assembly.GetType("System.Int32Extensions", throwOnError: true)!;
        var parse = extensions.GetMethod("parse", BindingFlags.Public | BindingFlags.Static, binder: null, types: [typeof(string)], modifiers: null)!;

        var result = parse.Invoke(null, ["42"])!;
        var resultType = result.GetType();
        var okType = GetCaseTypeFromTryGetValue(resultType, "Ok");
        var tryGetOk = GetTryGetValueMethod(resultType, okType);
        var args = new object?[] { null };

        var ok = (bool)(tryGetOk.Invoke(result, args) ?? false);

        Assert.True(ok);
        Assert.NotNull(args[0]);

        var value = args[0]!.GetType().GetProperty("Value")!.GetValue(args[0]);
        Assert.Equal(42, value);
    }

    [Fact]
    public void IntParse_ReturnsInvalidFormatError_ForBadInput()
    {
        var assembly = LoadRavenCoreAssembly();
        var extensions = assembly.GetType("System.Int32Extensions", throwOnError: true)!;
        var parse = extensions.GetMethod("parse", BindingFlags.Public | BindingFlags.Static, binder: null, types: [typeof(string)], modifiers: null)!;

        var result = parse.Invoke(null, ["foo"])!;
        var resultType = result.GetType();
        var errorType = GetCaseTypeFromTryGetValue(resultType, "Error");
        var tryGetError = GetTryGetValueMethod(resultType, errorType);
        var args = new object?[] { null };

        var ok = (bool)(tryGetError.Invoke(result, args) ?? false);

        Assert.True(ok);
        Assert.NotNull(args[0]);

        var errorCase = args[0]!;
        var error = errorCase.GetType().GetProperty("Data")!.GetValue(errorCase)!;
        var kind = error.GetType().GetProperty("Kind")!.GetValue(error);
        Assert.Equal("InvalidFormat", kind!.ToString());
    }

    private static Assembly LoadRavenCoreAssembly()
    {
        var path = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        return Assembly.LoadFrom(path);
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
}
