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
        let x = if flag {
            return 42
        } else {
            return ()
        }
    }
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1900").WithLocation(4, 13),
                new DiagnosticResult("RAV1900").WithLocation(6, 13)
            ]);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        var union = Assert.IsAssignableFrom<IUnionTypeSymbol>(local.Type);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Unit);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitReturnInIfExpression_GlobalInitializer_ProducesDiagnostics()
    {
        var code = """
let x = if true {
    return 42
} else {
    return ()
}
""";

        var verifier = CreateVerifier(code,
            expectedDiagnostics: [
                new DiagnosticResult("RAV1900").WithLocation(2, 5),
                new DiagnosticResult("RAV1900").WithLocation(4, 5)
            ]);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        var union = Assert.IsAssignableFrom<IUnionTypeSymbol>(local.Type);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Int32);
        Assert.Contains(union.Types, t => t.SpecialType == SpecialType.System_Unit);

        verifier.Verify();
    }

    [Fact]
    public void ExplicitReturnInIfExpressionStatementIsAllowed()
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
