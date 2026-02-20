using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class InferredUnionConsistencyTests : DiagnosticTestBase
{
    [Fact]
    public void MatchExpression_WithAbruptArms_InfersNonAbruptValueType()
    {
        const string code = """
import System.*

func Test(y: int) -> int {
    val r = y match {
        0 => return 0
        1 => 42
        _ => throw Exception("x")
    }

    return r + 1
}
""";

        var verifier = CreateVerifier(code, disabledDiagnostics: ["RAV9013"]);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var local = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "r");
        var symbol = (ILocalSymbol)model.GetDeclaredSymbol(local)!;

        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithLiteralValueArms_InfersLiteralUnion()
    {
        const string code = """
val state = 1 match {
    1 => "on"
    _ => "off"
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var local = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "state");
        var symbol = (ILocalSymbol)model.GetDeclaredSymbol(local)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(symbol.Type);

        Assert.Contains(union.Types, t => t is LiteralTypeSymbol literal && Equals(literal.ConstantValue, "on"));
        Assert.Contains(union.Types, t => t is LiteralTypeSymbol literal && Equals(literal.ConstantValue, "off"));

        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithHomogeneousValueArms_DoesNotInferUnion()
    {
        const string code = """
val seed = 0
val value = 1 match {
    1 => seed
    _ => 42
}
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var locals = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var symbol = (ILocalSymbol)model.GetDeclaredSymbol(locals[1])!;

        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void IfExpression_WithDistinctReferenceBranches_InfersTypeUnion()
    {
        const string code = """
class A {}
class B {}

val value = if true { A() } else { B() }
""";

        var verifier = CreateVerifier(code);
        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var local = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "value");
        var symbol = (ILocalSymbol)model.GetDeclaredSymbol(local)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(symbol.Type);

        Assert.Contains(union.Types, t => t is INamedTypeSymbol type && type.Name == "A");
        Assert.Contains(union.Types, t => t is INamedTypeSymbol type && type.Name == "B");

        verifier.Verify();
    }

    [Fact]
    public void LiteralStringUnion_IsImplicitlyConvertibleToUnderlyingString()
    {
        const string code = """
func Accept(text: string) -> int {
    return text.Length
}

val mode: "on" | "off" = "on"
val copy: string = mode
val size = Accept(mode)
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void LiteralIntUnion_IsImplicitlyConvertibleToUnderlyingInt()
    {
        const string code = """
func Accept(value: int) -> int {
    return value + 1
}

val mode: 1 | 2 = 1
val copy: int = mode
val next = Accept(mode)
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void AliasLiteralStringUnion_IsImplicitlyConvertibleToUnderlyingString()
    {
        const string code = """
alias Switch = "on" | "off"

func Read() -> Switch {
    return "on"
}

func Accept(text: string) -> int {
    return text.Length
}

val mode: Switch = Read()
val copy: string = mode
val size = Accept(mode)
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void AliasLiteralIntUnion_IsImplicitlyConvertibleToUnderlyingInt()
    {
        const string code = """
alias Digits = 1 | 2

func Read() -> Digits {
    return 1
}

func Accept(value: int) -> int {
    return value + 1
}

val digit: Digits = Read()
val copy: int = digit
val next = Accept(digit)
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }
}
