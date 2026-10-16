using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class CheckedConversionTests
{
    [Theory]
    [InlineData("int", "byte", 256)]
    [InlineData("int", "sbyte", 128)]
    [InlineData("int", "short", 32768)]
    [InlineData("int", "ushort", -1)]
    [InlineData("long", "int", 2147483648L)]
    [InlineData("int", "uint", -1)]
    [InlineData("long", "ulong", -1L)]
    [InlineData("uint", "int", uint.MaxValue)]
    [InlineData("ulong", "long", ulong.MaxValue)]
    [InlineData("double", "int", double.NaN)]
    [InlineData("double", "int", double.PositiveInfinity)]
    [InlineData("double", "int", 2147483648.0)]
    [InlineData("double", "uint", -1.0)]
    [InlineData("float", "ushort", 65536.0f)]
    [InlineData("double", "ulong", -1.0)]
    public void OutOfRangeThrows(string source, string target, object value)
    {
        using var loaded = Load($"class Runner {{ static func Run(a: {source}) -> {target} => checked(({target})a) }}");
        Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() =>
            loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [value])).InnerException);
    }

    [Theory]
    [InlineData("int", "byte", 255, (byte)255)]
    [InlineData("int", "sbyte", -128, (sbyte)-128)]
    [InlineData("long", "int", 2147483647L, int.MaxValue)]
    [InlineData("uint", "long", uint.MaxValue, 4294967295L)]
    [InlineData("ulong", "long", 42UL, 42L)]
    [InlineData("double", "int", -42.75, -42)]
    [InlineData("int", "double", -42, -42.0)]
    public void InRangePreservesValue(string source, string target, object value, object expected)
    {
        var compilation = Create($"class Runner {{ static func Run(a: {source}) -> {target} => checked(({target})a) }}");
        using var image = Emit(compilation);
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [value]));
        var tree = compilation.SyntaxTrees.Single();
        var cast = tree.GetRoot().DescendantNodes().OfType<CastExpressionSyntax>().Single();
        Assert.True(Assert.IsAssignableFrom<IConversionOperation>(compilation.GetSemanticModel(tree).GetOperation(cast)).IsChecked);
    }

    [Theory]
    [InlineData("unchecked((byte)a)", false)]
    [InlineData("checked(unchecked((byte)a))", false)]
    [InlineData("unchecked(checked((byte)a))", true)]
    public void NearestContextWins(string expression, bool overflows)
    {
        using var loaded = Load($"class Runner {{ static func Run(a: int) -> byte => {expression} }}");
        var method = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        if (overflows)
            Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [256])).InnerException);
        else
            Assert.Equal((byte)0, method.Invoke(null, [256]));
    }

    [Theory]
    [InlineData("checked((int)a)", 42)]
    [InlineData("unchecked((int)a)", 1)]
    [InlineData("checked(unchecked((int)a))", 1)]
    public void UserDefinedPairSelection(string expression, int expected)
    {
        using var loaded = Load($$"""
            class Box {
                static func explicit(value: Box) -> int => 1
                static func checked explicit(value: Box) -> int => 42
            }
            class Runner { static func Run() -> int { let a = Box(); return {{expression}} } }
            """);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void UnpairedOrdinaryConversionStillCompetes()
    {
        using var loaded = Load("""
            class Box {
                static func explicit(value: Box) -> int => 1
                static func checked explicit(value: Box) -> int => 2
                static func explicit(value: Box) -> long => 42
            }
            class Runner { static func Run() -> long => checked((long)Box()) }
            """);
        Assert.Equal(42L, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void ImportedCheckedConversionIsSelected()
    {
        using var provider = Emit(Create("""
            public class Box {
                static func explicit(value: Box) -> int => 1
                static func checked explicit(value: Box) -> int => 42
            }
            """));
        var consumer = Create("class Runner { static func Run(a: Box) -> int => checked((int)a) }")
            .AddReferences(MetadataReference.CreateFromImage(provider.ToArray()));
        using var image = Emit(consumer);
        var tree = consumer.SyntaxTrees.Single();
        var cast = tree.GetRoot().DescendantNodes().OfType<CastExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IConversionOperation>(consumer.GetSemanticModel(tree).GetOperation(cast));
        Assert.Equal("op_CheckedExplicit", operation.Conversion.MethodSymbol!.MetadataName);
    }

    [Fact]
    public void NumericExpressionTreeChecksOverflow()
    {
        using var loaded = Load("""
            import System.*
            import System.Linq.Expressions.*
            class Runner {
                static func Run(a: int) -> byte {
                    let f: Expression<Func<int, byte>> = (value: int) => checked((byte)value)
                    return f.Compile()(a)
                }
            }
            """);
        var method = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        Assert.Equal((byte)42, method.Invoke(null, [42]));
        Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [256])).InnerException);
    }

    [Fact]
    public void OperandIsEvaluatedOnce()
    {
        using var loaded = Load("""
            class Runner {
                static var Count: int = 0
                static func Get() -> int { Count += 1; return 41 }
                static func Run() -> int {
                    let value = checked((byte)Get())
                    return (int)value + Count
                }
            }
            """);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void NullableCheckedCastRemainsDiagnosed()
    {
        var compilation = Create("class Runner { static func Run(a: int?) -> byte? => checked((byte?)a) }");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0373");
    }

    [Fact]
    public void OrdinaryConversionsCanDifferByReturnType()
    {
        using var loaded = Load("""
            class Box {
                static func explicit(value: Box) -> int => 1
                static func explicit(value: Box) -> long => 42
            }
            class Runner { static func Run() -> long => (long)Box() }
            """);
        Assert.Equal(42L, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void DuplicateConversionStillRejected()
    {
        var compilation = Create("""
            class Box {
                static func explicit(value: Box) -> int => 1
                static func explicit(value: Box) -> int => 2
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0111");
    }

    [Theory]
    [InlineData("sbyte")]
    [InlineData("byte")]
    [InlineData("short")]
    [InlineData("ushort")]
    [InlineData("int")]
    [InlineData("uint")]
    [InlineData("long")]
    [InlineData("ulong")]
    public void DecimalUsesItsCheckedConversionContract(string target)
    {
        using var loaded = Load($"class Runner {{ static func Run(a: decimal) -> {target} => checked(({target})a) }}");
        var method = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        Assert.Equal(42m, Convert.ToDecimal(method.Invoke(null, [42.75m])));
        Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [decimal.MaxValue])).InnerException);
    }

    private static Compilation Create(string source) => Compilation.Create("CheckedConversion_" + Guid.NewGuid().ToString("N"),
        new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
        .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);

    private static MemoryStream Emit(Compilation compilation)
    {
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        return image;
    }

    private static TestAssemblyLoader.LoadedAssembly Load(string source)
    {
        using var image = Emit(Create(source));
        return TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
    }
}
