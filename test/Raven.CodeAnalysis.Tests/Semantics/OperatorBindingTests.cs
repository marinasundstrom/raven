using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class OperatorBindingTests : CompilationTestBase
{
    [Fact]
    public void OperatorDeclaration_BindsUserDefinedOperatorSymbol()
    {
        var source = """
class Number
{
    public static operator +(left: Number, right: Number) -> Number { return left }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().Single();

        var symbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(MethodKind.UserDefinedOperator, symbol.MethodKind);
        Assert.Equal("op_Addition", symbol.Name);
        Assert.True(symbol.IsStatic);
        Assert.Equal(2, symbol.Parameters.Length);
        Assert.Same(symbol.ContainingType, symbol.ReturnType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void OperatorDeclaration_MissingStatic_ReportsDiagnostic()
    {
        var source = """
class Number
{
    public operator +(left: Number, right: Number) -> Number { return left }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        Assert.Contains(
            compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.OperatorMustBeStatic);
    }

    [Fact]
    public void OperatorDeclaration_InvalidParameterCount_ReportsDiagnostic()
    {
        var source = """
class Counter
{
    public static operator ++(left: Counter, right: Counter) -> Counter { return left }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        Assert.Contains(
            compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.OperatorParameterCountInvalid);
    }

    [Fact]
    public void ExtensionOperator_BinaryExpressionBindsUserDefinedOperator()
    {
        var source = """
namespace Sample {
    class Number { }

    extension NumberOps for Number {
        public static operator +(left: Number, right: Number) -> Number { return left }
    }

    val a = Number()
    val b = Number()
    val c = a + b
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<BinaryExpressionSyntax>().Single();

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(expression).Symbol);

        Assert.Equal(MethodKind.UserDefinedOperator, symbol.MethodKind);
        Assert.Equal("op_Addition", symbol.Name);
        Assert.Equal("NumberOps", symbol.ContainingType?.Name);
    }

    [Fact]
    public void OperatorUsage_BinaryExpressionBindsUserDefinedOperator()
    {
        var source = """
class Number
{
    public static operator +(left: Number, right: Number) -> Number { return left }
}

val a = Number()
val b = Number()
val c = a + b
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<BinaryExpressionSyntax>().Single();

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(expression).Symbol);

        Assert.Equal(MethodKind.UserDefinedOperator, symbol.MethodKind);
        Assert.Equal("op_Addition", symbol.Name);
    }

    [Fact]
    public void OperatorUsage_UnaryExpressionBindsUserDefinedOperator()
    {
        var source = """
class Number
{
    public static operator -(value: Number) -> Number { return value }
}

val a = Number()
val b = -a
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var expression = tree.GetRoot().DescendantNodes().OfType<UnaryExpressionSyntax>().Single();

        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(expression).Symbol);

        Assert.Equal(MethodKind.UserDefinedOperator, symbol.MethodKind);
        Assert.Equal("op_UnaryNegation", symbol.Name);
    }

    [Fact]
    public void OperatorDeclaration_BitwiseOperatorsBindExpectedMetadataNames()
    {
        var source = """
class Bits
{
    public static operator ~(value: Bits) -> Bits { return value }
    public static operator <<(left: Bits, right: int) -> Bits { return left }
    public static operator >>(left: Bits, right: int) -> Bits { return left }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var operators = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().ToArray();

        Assert.Equal(3, operators.Length);

        var symbols = operators
            .Select(op => Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(op)))
            .ToArray();

        Assert.Contains(symbols, symbol => symbol.Name == "op_OnesComplement");
        Assert.Contains(symbols, symbol => symbol.Name == "op_LeftShift");
        Assert.Contains(symbols, symbol => symbol.Name == "op_RightShift");
    }

    [Fact]
    public void OperatorUsage_NullableMixedEquality_BindsWithoutOperatorDiagnostic()
    {
        var source = """
class Number
{
    public static operator ==(left: Number, right: Number) -> bool { return true }
    public static operator !=(left: Number, right: Number) -> bool { return false }
}

val a: Number? = null
val b = Number()
val eq = a == b
val ne = a != b
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var expressions = tree.GetRoot().DescendantNodes().OfType<BinaryExpressionSyntax>().ToArray();

        Assert.Equal(2, expressions.Length);
        Assert.All(expressions, expression => Assert.Equal(SpecialType.System_Boolean, model.GetTypeInfo(expression).Type?.SpecialType));
        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.OperatorCannotBeAppliedToOperandsOfTypes);
    }

    [Fact]
    public void NullCheck_WithUserDefinedEquality_DoesNotNarrow()
    {
        var source = """
class Number
{
    public Ping() -> unit { }

    public static operator ==(left: Number?, right: Number?) -> bool { return true }
    public static operator !=(left: Number?, right: Number?) -> bool { return false }
}

class C
{
    Run() -> unit
    {
        val x: Number? = null
        if x != null {
            x.Ping()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Descriptor == CompilerDiagnostics.PossibleNullReferenceAccess);
    }
}
