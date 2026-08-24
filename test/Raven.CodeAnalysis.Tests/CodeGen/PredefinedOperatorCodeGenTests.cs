using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class PredefinedOperatorCodeGenTests
{
    [Fact]
    public void BitwiseSmallIntegralOperands_PromoteAndConvertWithoutGenericMathCall()
    {
        const string code = """
class BitwiseValues {
    func Combine(a: byte, b: byte) -> int {
        return a | b
    }

    func Clear(a: byte, b: byte) -> int {
        return a & ~b
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("predefined_bitwise_codegen", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(
            result.Success,
            string.Join(Environment.NewLine, result.Diagnostics.Select(diagnostic => $"{diagnostic.Location.SourceSpan}: {diagnostic}")));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var type = loaded.Assembly.GetType("BitwiseValues", throwOnError: true)!;
        var instance = Activator.CreateInstance(type)!;
        var combine = type.GetMethod("Combine", BindingFlags.Instance | BindingFlags.Public)!;
        var clear = type.GetMethod("Clear", BindingFlags.Instance | BindingFlags.Public)!;

        Assert.Equal(11, (int)combine.Invoke(instance, [(byte)1, (byte)10])!);
        Assert.Equal(1, (int)clear.Invoke(instance, [(byte)3, (byte)2])!);
    }
}
