using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class ExplicitReturnInIfExpressionTests : DiagnosticTestBase
{
    [Fact]
    public void ExplicitReturnInIfExpressionInitializerProducesDiagnostics()
    {
        var code = """
class Foo {
    Test(flag: bool) -> int | () {
        val x = if flag {
            return 42
        } else {
            return ()
        }
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1900").WithSpan(4, 13, 4, 22),
                new DiagnosticResult("RAV1900").WithSpan(6, 13, 6, 22)
            ]);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Unit);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitReturnInIfExpression_GlobalInitializer_ProducesDiagnostics()
    {
        var code = """
val x = if true {
    return 42
} else {
    return ()
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1900").WithSpan(2, 5, 2, 14),
                        new DiagnosticResult("RAV1900").WithSpan(4, 5, 4, 14)
            ]);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        var union = Assert.IsAssignableFrom<ITypeUnionSymbol>(local.Type);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Unit);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitReturnInIfStatementIsAllowed()
    {
        var code = """
class Foo {
    Test(flag: bool) -> int | () {
        if flag {
            return 42
        } else {
            return ()
        }
    }
}
""";

        var verifier = CreateVerifier(code);

        verifier.Verify();
    }
}
