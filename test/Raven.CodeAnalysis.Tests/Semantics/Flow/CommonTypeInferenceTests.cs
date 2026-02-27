using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public sealed class CommonTypeInferenceTests : DiagnosticTestBase
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
    public void IfExpression_WithDistinctReferenceBranches_InfersCommonBaseType()
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

        Assert.Equal(SpecialType.System_Object, symbol.Type.SpecialType);
        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_WithHomogeneousValueArms_InfersConcreteType()
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
}
