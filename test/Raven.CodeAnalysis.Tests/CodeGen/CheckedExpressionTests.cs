using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis.Tests;

public class CheckedExpressionTests
{
    [Theory]
    [InlineData("int", "+", int.MaxValue, 1)]
    [InlineData("int", "-", int.MinValue, 1)]
    [InlineData("int", "*", int.MaxValue, 2)]
    [InlineData("long", "+", long.MaxValue, 1L)]
    [InlineData("long", "*", long.MaxValue, 2L)]
    [InlineData("uint", "+", uint.MaxValue, 1U)]
    [InlineData("ulong", "*", ulong.MaxValue, 2UL)]
    public void IntegerOverflowIsCheckedAtRuntime(string type, string op, object left, object right)
    {
        var compilation = Create($"class Runner {{ static func Run(a: {type}, b: {type}) -> {type} => checked(a {op} b) }}");
        using var image = Emit(compilation);
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var method = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [left, right])).InnerException);
        var syntax = compilation.SyntaxTrees.Single().GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IBinaryOperation>(compilation.GetSemanticModel(syntax.SyntaxTree).GetOperation(syntax));
        Assert.True(operation.IsChecked);
        var context = syntax.Parent!;
        var parenthesized = Assert.IsAssignableFrom<IParenthesizedOperation>(compilation.GetSemanticModel(syntax.SyntaxTree).GetOperation(context));
        Assert.NotNull(parenthesized.Operand);
    }

    [Theory]
    [InlineData("unchecked(a + b)", int.MinValue)]
    [InlineData("checked(unchecked(a + b))", int.MinValue)]
    [InlineData("unchecked(checked(a + b))", null)]
    public void NestedContextsUseNearestScope(string expression, object? expected)
    {
        var compilation = Create($"class Runner {{ static func Run(a: int, b: int) -> int => {expression} }}");
        using var image = Emit(compilation);
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var method = loaded.Assembly.GetType("Runner")!.GetMethod("Run")!;
        if (expected is null)
            Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() => method.Invoke(null, [int.MaxValue, 1])).InnerException);
        else Assert.Equal(expected, method.Invoke(null, [int.MaxValue, 1]));
    }

    [Theory]
    [InlineData("+", "checked(a + b)", 42)]
    [InlineData("-", "checked(a - b)", 42)]
    [InlineData("*", "checked(a * b)", 42)]
    [InlineData("/", "checked(a / b)", 42)]
    [InlineData("+", "unchecked(a + b)", 1)]
    [InlineData("+", "checked(unchecked(a + b))", 1)]
    public void CheckedOverloadSelection(string token, string expression, int expected)
    {
        var compilation = Create($$"""
            class Box {
                static func {{token}}(a: Box, b: Box) -> int => 1
                static func checked {{token}}(a: Box, b: Box) -> int => 42
            }
            class Runner {
                static func Run() -> int {
                    let a = Box()
                    let b = Box()
                    return {{expression}}
                }
            }
            """);
        using var image = Emit(compilation);
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void InapplicableCheckedOverloadFallsBackToOrdinary()
    {
        using var image = Emit(Create("""
            class Box {
                static func +(a: Box, b: int) -> int => 42
                static func +(a: Box, b: string) -> int => 1
                static func checked +(a: Box, b: string) -> int => 2
            }
            class Runner { static func Run() -> int => checked(Box() + 1) }
            """));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("checked(a++)")]
    [InlineData("checked(a += 1)")]
    public void UnsupportedCheckedOperationsFailClosed(string expression)
    {
        var compilation = Create($"class Runner {{ static func Run(a: int) {{ let result = {expression} }} }}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0373");
    }

    [Fact]
    public void CheckedAmbiguityDoesNotFallBackToBetterOrdinaryCandidate()
    {
        var compilation = Create("""
            class Box {
                static func +(a: Box, b: Box) -> int => 42
                static func +(a: Box, b: object) -> int => 1
                static func +(a: object, b: Box) -> int => 1
                static func checked +(a: Box, b: object) -> int => 2
                static func checked +(a: object, b: Box) -> int => 3
            }
            class Runner { static func Run() -> int => checked(Box() + Box()) }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0121");
    }

    [Fact]
    public void ImportedCheckedOperatorIsSelected()
    {
        using var provider = Emit(Create("""
            public class Box {
                static func +(a: Box, b: Box) -> int => 1
                static func checked +(a: Box, b: Box) -> int => 42
            }
            """));
        var consumer = Create("class Runner { static func Run(a: Box, b: Box) -> int => checked(a + b) }")
            .AddReferences(MetadataReference.CreateFromImage(provider.ToArray()));
        Assert.DoesNotContain(consumer.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var tree = consumer.SyntaxTrees.Single();
        var binary = tree.GetRoot().DescendantNodes().OfType<InfixOperatorExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(consumer.GetSemanticModel(tree).GetSymbolInfo(binary).Symbol);
        Assert.Equal("op_CheckedAddition", method.MetadataName);
        using var emittedConsumer = Emit(consumer);
    }

    [Fact]
    public void ContextDoesNotFlowIntoNestedFunctionBody()
    {
        using var image = Emit(Create("""
            class Runner {
                static func Run(a: int, b: int) -> int {
                    let f: () -> int = checked(() => a + b)
                    return f()
                }
            }
            """));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(int.MinValue, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [int.MaxValue, 1]));
    }

    [Fact]
    public void LiftedCheckedArithmeticFailsClosed()
    {
        var compilation = Create("class Runner { static func Run(a: int?, b: int?) -> int? => checked(a + b) }");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0373");
    }

    [Theory]
    [InlineData("int", int.MinValue)]
    [InlineData("long", long.MinValue)]
    public void CheckedNegationRejectsMinimumValue(string type, object minimum)
    {
        var compilation = Create($"class Runner {{ static func Run(a: {type}) -> {type} => checked(-a) }}");
        using var image = Emit(compilation);
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.IsType<OverflowException>(Assert.Throws<TargetInvocationException>(() =>
            loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [minimum])).InnerException);
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<PrefixOperatorExpressionSyntax>().Single();
        Assert.True(Assert.IsAssignableFrom<IUnaryOperation>(compilation.GetSemanticModel(tree).GetOperation(syntax)).IsChecked);
    }

    [Theory]
    [InlineData("checked(-a)", -42)]
    [InlineData("checked(unchecked(-a))", -42)]
    [InlineData("checked(-42)", -42)]
    public void CheckedNegationProducesResult(string expression, int expected)
    {
        using var image = Emit(Create($"class Runner {{ static func Run(a: int) -> int => {expression} }}"));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [42]));
    }

    [Theory]
    [InlineData("checked(-a)", 42)]
    [InlineData("unchecked(-a)", 1)]
    [InlineData("checked(unchecked(-a))", 1)]
    public void CheckedUnaryOverloadSelection(string expression, int expected)
    {
        using var image = Emit(Create($$"""
            class Box {
                static func -(a: Box) -> int => 1
                static func checked -(a: Box) -> int => 42
            }
            class Runner { static func Run() -> int { let a = Box(); return {{expression}} } }
            """));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void CheckedUnaryFallsBackToOrdinaryOperator()
    {
        using var image = Emit(Create("""
            class Box { static func -(a: Box) -> int => 42 }
            class Runner { static func Run() -> int => checked(-Box()) }
            """));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void ImportedCheckedUnaryOperatorIsSelected()
    {
        using var provider = Emit(Create("""
            public class Box {
                static func -(a: Box) -> int => 1
                static func checked -(a: Box) -> int => 42
            }
            """));
        var consumer = Create("class Runner { static func Run(a: Box) -> int => checked(-a) }")
            .AddReferences(MetadataReference.CreateFromImage(provider.ToArray()));
        var tree = consumer.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<PrefixOperatorExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(consumer.GetSemanticModel(tree).GetSymbolInfo(syntax).Symbol);
        Assert.Equal("op_CheckedUnaryNegation", method.MetadataName);
        using var image = Emit(consumer);
    }

    [Fact]
    public void UncheckedNegationWrapsMinimumValue()
    {
        using var image = Emit(Create("class Runner { static func Run(a: int) -> int => checked(unchecked(-a)) }"));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(int.MinValue, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [int.MinValue]));
    }

    [Fact]
    public void CheckedNegationPromotesSmallIntegers()
    {
        using var image = Emit(Create("class Runner { static func Run(a: sbyte) -> int => checked(-a) }"));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        Assert.Equal(128, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [sbyte.MinValue]));
    }

    [Fact]
    public void CheckedFloatingNegationPreservesNegativeZero()
    {
        var compilation = Create("class Runner { static func Run(a: double) -> double => checked(-a) }");
        using var image = Emit(compilation);
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var result = Assert.IsType<double>(loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, [0d]));
        Assert.Equal(long.MinValue, BitConverter.DoubleToInt64Bits(result));
        var tree = compilation.SyntaxTrees.Single();
        var syntax = tree.GetRoot().DescendantNodes().OfType<PrefixOperatorExpressionSyntax>().Single();
        Assert.False(Assert.IsAssignableFrom<IUnaryOperation>(compilation.GetSemanticModel(tree).GetOperation(syntax)).IsChecked);
    }

    [Fact]
    public void CheckedNegationInExpressionTreesPreservesOverflow()
    {
        var compilation = Create("""
            import System.Linq.Expressions.*
            class Runner {
                static func Run() -> Expression<System.Func<int, int>> => (a: int) => checked(-a)
            }
            """);
        using var image = Emit(compilation);
        using var loaded = TestAssemblyLoader.LoadFromStream(image, TestMetadataReferences.Default);
        var tree = Assert.IsAssignableFrom<System.Linq.Expressions.Expression<Func<int, int>>>(
            loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
        var function = tree.Compile();
        Assert.Equal(-42, function(42));
        Assert.Throws<OverflowException>(() => function(int.MinValue));
    }

    private static MemoryStream Emit(Compilation compilation)
    {
        var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        return image;
    }

    private static Compilation Create(string source) => Compilation.Create("CheckedExpressions" + Guid.NewGuid().ToString("N"),
        new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
        .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
}
