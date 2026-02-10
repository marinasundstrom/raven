using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class ConversionOperatorCodeGenTests
{
    [Fact]
    public void ExplicitConversionOperator_EmitsInvokableMethod()
    {
        const string code = """
class NumberBox {
    public Value: int { get { return 42; } }
    public static explicit operator(value: NumberBox) -> int { return value.Value }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
            "conversion-op",
            [syntaxTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
        var assembly = loaded.Assembly;

        var numberBoxType = assembly.GetType("NumberBox", throwOnError: true)!;
        var conversion = numberBoxType.GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Single(method => method.Name == "op_Explicit");
        var instance = Activator.CreateInstance(numberBoxType)!;
        var converted = (int)conversion.Invoke(null, new[] { instance })!;

        Assert.Equal(42, converted);
    }

    [Fact]
    public void ImplicitConversionOperator_ToNullableType_EmitsValidIL()
    {
        var ravenCoreReference = CreateRavenCoreOptionReference(out var ravenCorePath);

        const string code = """
        import System.*

        class OptionConversionRunner {
            public static RunReference() -> string? {
                val opt1 = Option<string>.Some("OK")
                val opt12: string? = opt1
                return opt12
            }

            public static RunValue() -> int? {
                val opt2 = Option<int>.Some(42)
                val opt22: int? = opt2
                return opt22
            }
        }
        """;

        try
        {
            var syntaxTree = SyntaxTree.ParseText(code);
            var compilation = Compilation.Create(
                "option-conversion",
                [syntaxTree],
                [.. TestMetadataReferences.Default, ravenCoreReference],
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            using var peStream = new MemoryStream();
            var result = compilation.Emit(peStream);
            Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

            using var loaded = TestAssemblyLoader.LoadFromStream(peStream, [.. TestMetadataReferences.Default, ravenCoreReference]);
            var assembly = loaded.Assembly;

            var runnerType = assembly.GetType("OptionConversionRunner", throwOnError: true)!;
            var referenceMethod = runnerType.GetMethod("RunReference", BindingFlags.Public | BindingFlags.Static)!;
            var valueMethod = runnerType.GetMethod("RunValue", BindingFlags.Public | BindingFlags.Static)!;
            var referenceResult = (string?)referenceMethod.Invoke(null, Array.Empty<object>());
            var valueResult = (int?)valueMethod.Invoke(null, Array.Empty<object>());

            Assert.Equal("OK", referenceResult);
            Assert.Equal(42, valueResult);
        }
        finally
        {
            if (File.Exists(ravenCorePath))
                File.Delete(ravenCorePath);
        }
    }

    private static PortableExecutableReference CreateRavenCoreOptionReference(out string assemblyPath)
    {
        const string fixtureSource = """
namespace System

public union Option<T> {
    Some(value: T)
    None
}

public extension OptionExtensions1<T : class> for Option<T> {
    public static implicit operator(opt: Option<T>) -> T? {
        if opt is .Some(val value) {
            return value
        }
        null
    }
}

public extension OptionExtensions2<T : struct> for Option<T> {
    public static implicit operator(opt: Option<T>) -> T? {
        if opt is .Some(val value) {
            return value
        }
        null
    }
}
""";

        var fixtureTree = SyntaxTree.ParseText(fixtureSource);
        var ravenCoreCompilation = Compilation.Create(
            "raven-core-option-fixture",
            [fixtureTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var ravenCoreStream = new MemoryStream();
        var ravenCoreEmit = ravenCoreCompilation.Emit(ravenCoreStream);
        Assert.True(ravenCoreEmit.Success, string.Join(Environment.NewLine, ravenCoreEmit.Diagnostics));

        assemblyPath = Path.Combine(Path.GetTempPath(), $"raven-core-option-fixture-{Guid.NewGuid():N}.dll");
        File.WriteAllBytes(assemblyPath, ravenCoreStream.ToArray());

        return MetadataReference.CreateFromFile(assemblyPath);
    }
}
