using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ConstantOverflowTests
{
    [Theory]
    [InlineData("2147483647 + 1")]
    [InlineData("checked(2147483647 + 1)")]
    [InlineData("unchecked(checked(2147483647 + 1))")]
    [InlineData("-2147483648 - 1")]
    [InlineData("2147483647 * 2")]
    [InlineData("9223372036854775807L + 1L")]
    [InlineData("(uint)4294967295L + (uint)1")]
    [InlineData("unchecked((ulong)-1) + (ulong)1")]
    [InlineData("-(-2147483648)")]
    [InlineData("(byte)256")]
    [InlineData("unchecked(79228162514264337593543950335m + 1m)")]
    [InlineData("checked((int)2147483648.0)")]
    [InlineData("-2147483648 / -1")]
    public void ConstantOverflowIsADiagnostic(string expression)
    {
        var compilation = Create($"class Runner {{ static func Run() {{ let value = {expression} }} }}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0374");
    }

    [Theory]
    [InlineData("unchecked(2147483647 + 1)", int.MinValue)]
    [InlineData("checked(unchecked(2147483647 + 1))", int.MinValue)]
    [InlineData("unchecked(-(-2147483648))", int.MinValue)]
    [InlineData("unchecked((byte)256)", (byte)0)]
    [InlineData("(byte)(40 + 2)", (byte)42)]
    [InlineData("40 + 2", 42)]
    [InlineData("84 / 2", 42)]
    [InlineData("unchecked(-2147483648 / -1)", int.MinValue)]
    [InlineData("-85 % 43", -42)]
    [InlineData("(40 | 3) ^ 1", 42)]
    [InlineData("(127 & 63) - 21", 42)]
    [InlineData("~(-43)", 42)]
    [InlineData("checked(1 << 31)", int.MinValue)]
    [InlineData("1 << 32", 1)]
    [InlineData("1 << -1", int.MinValue)]
    [InlineData("-1 >> 31", -1)]
    [InlineData("-1 >>> 31", 1)]
    [InlineData("1L << 64", 1L)]
    [InlineData("-1L >>> 63", 1L)]
    [InlineData("-2147483648 % -1", 0)]
    public void ConstantLocalsRetainTypedValues(string expression, object expected)
    {
        var compilation = Create($"class Runner {{ static func Run() {{ const answer = {expression} }} }}");
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = compilation.SyntaxTrees.Single();
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<ILocalSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declarator));
        Assert.Equal(expected, symbol.ConstantValue);
    }

    [Fact]
    public void ConstantFieldsAndDependenciesEvaluate()
    {
        var compilation = Create("""
            class Runner {
                const Base = 40
                const Answer = Base + 2
                static func Run() -> int => Answer
            }
            """);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("unchecked { const answer = 2147483647 + 1 }", false)]
    [InlineData("checked { const answer = 2147483647 + 1 }", true)]
    [InlineData("unchecked { func F() -> int => 2147483647 + 1 }", true)]
    public void BlocksAndFunctionBoundariesApply(string body, bool overflow)
    {
        var diagnostics = Create($"class Runner {{ static func Run() {{ {body} }} }}").GetDiagnostics();
        Assert.Equal(overflow, diagnostics.Any(d => d.Id == "RAV0374"));
    }

    [Theory]
    [InlineData("1 / 0")]
    [InlineData("1 % 0")]
    [InlineData("unchecked(1 / 0)")]
    [InlineData("1m / 0m")]
    public void ConstantZeroDivisorIsDiagnosed(string expression)
    {
        var compilation = Create($"class Runner {{ static func Run() {{ let value = {expression} }} }}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0375");
    }

    [Fact]
    public void ArithmeticParameterDefaultIsEmittedAndInvoked()
    {
        var compilation = Create("""
            class Runner {
                static func Answer(value: int = (84 / 2) | 0) -> int => value
                static func Run() -> int => Answer()
            }
            """);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Runner")!;
        Assert.Equal(42, type.GetMethod("Run")!.Invoke(null, null));
        Assert.Equal(42, type.GetMethod("Answer")!.GetParameters()[0].DefaultValue);
    }

    [Fact]
    public void OverflowingParameterDefaultIsDiagnosed()
    {
        var compilation = Create("class Runner { static func Run(value: int = 2147483647 + 1) {} }");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0374");
    }

    private static Compilation Create(string source) => Compilation.Create("ConstantOverflow_" + Guid.NewGuid().ToString("N"),
        new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
        .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
}
