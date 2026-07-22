using System;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public sealed class ParseExtensionsTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void TryParseProjection_EmitsThroughAttributedBridge()
    {
        const string code = """
import System.*
import System.Globalization.*

func TryParseProjected(text: string) -> Option<int> {
    return int.TryParse(text)
}

func TryParseProjectedWithStyle(text: string) -> Option<int> {
    return int.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture)
}

func TryParseDateTimeProjectedWithStyles(text: string) -> Option<DateTime> {
    return DateTime.TryParse(text, CultureInfo.InvariantCulture, DateTimeStyles.None)
}

func ParseProjected(text: string) -> Result<int, ParseIntError> {
    return int.Parse(text)
}
""";

        CreateVerifier(code).Verify();
    }

    [Theory]
    [InlineData("System.Int32Extensions", typeof(int), "42")]
    [InlineData("System.Int64Extensions", typeof(long), "42")]
    [InlineData("System.DoubleExtensions", typeof(double), "42")]
    [InlineData("System.DecimalExtensions", typeof(decimal), "42")]
    [InlineData("System.GuidExtensions", typeof(Guid), "d2719b1e-88c5-4a06-aeba-69d19e70b9f7")]
    [InlineData("System.DateTimeExtensions", typeof(DateTime), "2026-07-22")]
    public void TryParseProjection_ReturnsSome_ForValidInput(string containerName, Type valueType, string input)
    {
        var assembly = LoadRavenCoreAssembly();
        var extensions = assembly.GetType(containerName, throwOnError: true)!;
        var tryParse = extensions.GetMethod(
            "TryParse",
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            types: [typeof(string)],
            modifiers: null)!;

        var option = tryParse.Invoke(null, [input])!;
        var optionType = option.GetType();
        var someType = GetCaseTypeFromTryGetValue(optionType, "Some");
        var tryGetSome = GetTryGetValueMethod(optionType, someType);
        var args = new object?[] { null };

        Assert.True((bool)(tryGetSome.Invoke(option, args) ?? false));
        Assert.NotNull(args[0]);
        Assert.IsType(valueType, args[0]!.GetType().GetProperty("Value")!.GetValue(args[0]));
    }

    [Fact]
    public void TryParseProjection_ReturnsNone_ForInvalidInput()
    {
        var assembly = LoadRavenCoreAssembly();
        var extensions = assembly.GetType("System.Int32Extensions", throwOnError: true)!;
        var tryParse = extensions.GetMethod(
            "TryParse",
            BindingFlags.Public | BindingFlags.Static,
            binder: null,
            types: [typeof(string)],
            modifiers: null)!;

        var option = tryParse.Invoke(null, ["not-an-int"])!;
        var optionType = option.GetType();
        var someType = GetCaseTypeFromTryGetValue(optionType, "Some");
        var tryGetSome = GetTryGetValueMethod(optionType, someType);
        var args = new object?[] { null };

        Assert.False((bool)(tryGetSome.Invoke(option, args) ?? false));
    }

    [Fact]
    public void IntParse_BindsFromRavenCore_AndSupportsResultPropagation()
    {
        const string code = """
import System.*

func ParseMain(text: string) -> Result<int, IParseError> {
    val value = int.parse(text)?
    return .Ok(value)
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

    [Fact]
    public void WithMessage_OnTypedError_PreservesInnerErrorType()
    {
        const string code = """
import System.*
import System.Globalization.*

func Wrap() -> string {
    val error = ParseIntError(.InvalidFormat, "foo", NumberStyles.Integer)
    val wrapped = error.WithContext("wrapped")
    return if wrapped.Cause.Kind == IntErrorKind.InvalidFormat { "ok" } else { "bad" }
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void ContextError_ExplicitInterfaceCause_CoexistsWithTypedCause()
    {
        const string code = """
import System.*
import System.Globalization.*

func Wrap() -> string {
    val wrapped = ParseIntError(.InvalidFormat, "foo", NumberStyles.Integer).WithContext("wrapped")
    val erased: IError = wrapped
    return "$wrapped.Cause.Kind|$erased.Cause?.Message"
}
""";

        CreateVerifier(code).Verify();
    }

    [Fact]
    public void WithMessage_OnResult_ProjectsErrorChannelToContextError()
    {
        const string code = """
import System.*

func Wrap(text: string) -> Result<int, ContextError<ParseIntError>> {
    return int.parse(text).WithContext("wrapped")
}
""";

        CreateVerifier(code).Verify();
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
