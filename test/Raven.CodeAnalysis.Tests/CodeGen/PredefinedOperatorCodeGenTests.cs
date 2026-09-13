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
    public void UnsignedArithmetic_PreservesMagnitudeAndZeroFillsRightShift()
    {
        const string source = """
class Arithmetic {
    func Divide32(a: uint, b: uint) -> uint { return a / b }
    func Remainder32(a: uint, b: uint) -> uint { return a % b }
    func Shift32(a: uint, count: int) -> uint { return a >> count }
    func Divide64(a: ulong, b: ulong) -> ulong { return a / b }
    func Remainder64(a: ulong, b: ulong) -> ulong { return a % b }
    func Shift64(a: ulong, count: int) -> ulong { return a >> count }
    func Signed(a: long, count: int) -> long { return a >> count }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("unsigned_arithmetic", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Arithmetic", true)!;
        var instance = Activator.CreateInstance(type)!;
        object? Invoke(string name, object left, object right) => type.GetMethod(name)!.Invoke(instance, [left, right]);
        Assert.Equal(uint.MaxValue / 2, Invoke("Divide32", uint.MaxValue, 2u));
        Assert.Equal(uint.MaxValue % 2, Invoke("Remainder32", uint.MaxValue, 2u));
        Assert.Equal(uint.MaxValue >> 1, Invoke("Shift32", uint.MaxValue, 1));
        Assert.Equal(ulong.MaxValue / 2, Invoke("Divide64", ulong.MaxValue, 2UL));
        Assert.Equal(ulong.MaxValue % 2, Invoke("Remainder64", ulong.MaxValue, 2UL));
        Assert.Equal(ulong.MaxValue >> 1, Invoke("Shift64", ulong.MaxValue, 1));
        Assert.Equal(-1L, Invoke("Signed", -1L, 1));
    }

    [Fact]
    public void NumericComparisons_PreserveUnsignedAndUnorderedSemantics()
    {
        (string Type, object Left, object Right, bool[] Expected)[] cases =
        [
            ("uint", uint.MaxValue, 1u, [false, true, false, true, false, true]),
            ("ulong", ulong.MaxValue, 1UL, [false, true, false, true, false, true]),
            ("int", int.MinValue, int.MaxValue, [false, true, true, false, true, false]),
            ("long", long.MinValue, long.MaxValue, [false, true, true, false, true, false]),
            ("double", double.NaN, 1.0, [false, true, false, false, false, false]),
            ("double", 1.0, double.NaN, [false, true, false, false, false, false]),
            ("double", -0.0, 0.0, [true, false, false, false, true, true]),
            ("double", double.PositiveInfinity, 1.0, [false, true, false, true, false, true]),
            ("float", float.NaN, 1.0f, [false, true, false, false, false, false])
        ];
        string[] operators = ["==", "!=", "<", ">", "<=", ">="];
        foreach (var (typeName, left, right, expected) in cases)
        {
            var methods = operators.Select((op, index) =>
                $"func Compare{index}(a: {typeName}, b: {typeName}) -> bool {{ return a {op} b }}");
            var tree = SyntaxTree.ParseText("class Comparisons {\n" + string.Join("\n", methods) + "\n}");
            var references = TestMetadataReferences.Default;
            var compilation = Compilation.Create("comparisons_" + Guid.NewGuid().ToString("N"),
                    new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddSyntaxTrees(tree).AddReferences(references);
            using var stream = new MemoryStream();
            var result = compilation.Emit(stream);
            Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
            var type = loaded.Assembly.GetType("Comparisons", true)!;
            var instance = Activator.CreateInstance(type)!;
            for (var index = 0; index < operators.Length; index++)
            {
                var actual = (bool)type.GetMethod($"Compare{index}")!.Invoke(instance, [left, right])!;
                Assert.True(actual == expected[index], $"{typeName}: {left} {operators[index]} {right}");
            }
        }
    }

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
