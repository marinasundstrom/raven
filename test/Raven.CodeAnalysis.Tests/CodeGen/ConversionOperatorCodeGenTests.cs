using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;
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
}
