using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class BlockExpressionEarlyExitTests : DiagnosticTestBase
{
    [Fact]
    public void IfExpression_InitializerWithReturnExpressions_DoesNotContributeAbruptBranchesToValueType()
    {
        const string code = """
class Foo {
    func Test(flag: bool) {
        let x = if flag {
            return 42
        } else {
            return ()
        }
    }
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1503").WithAnySpan().WithArguments("int", "()")
            ]);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        Assert.Equal(SpecialType.System_Unit, local.Type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void IfExpression_GlobalInitializerWithReturnExpressions_HasUnitType()
    {
        const string code = """
let x = if true {
    return 42
} else {
    return ()
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1503").WithAnySpan().WithArguments("int", "()")
            ]);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var symbol = model.GetDeclaredSymbol(variable)!;
        var type = symbol switch
        {
            ILocalSymbol local => local.Type,
            IFieldSymbol field => field.Type,
            _ => throw new InvalidOperationException($"Unexpected symbol: {symbol.GetType().Name}")
        };
        Assert.Equal(SpecialType.System_Unit, type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void LocalInitializer_BlockExpressionWithReturnStatement_IsAllowed()
    {
        const string code = """
class C {
    func M(f: bool) -> bool {
        let x = {
            if f {
                return true
            }

            42
        }

        return false
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void NullCoalesce_RightBlockExpressionWithReturnStatement_IsAllowed()
    {
        const string code = """
class C {
    func M(obj: string?) -> () {
        let foo = obj ?? {
            return ()
        }

        ()
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void IfStatement_WithReturnStatements_IsAllowed()
    {
        const string code = """
class Foo {
    func Test(flag: bool) -> int {
        if flag {
            return 42
        } else {
            return 0
        }
    }
}
""";

        var verifier = CreateVerifier(code);
        verifier.Verify();
    }

    [Fact]
    public void MatchExpression_BlockArmWithReturnExpression_IsAllowed()
    {
        const string code = """
class C {
    func M(value: int) -> int {
        let x = match value {
            0 => {
                return 1
            }
            _ => 2
        }

        return x
    }
}
""";

        var verifier = CreateVerifier(code);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(variable));
        Assert.Equal(SpecialType.System_Int32, local.Type.SpecialType);

        verifier.Verify();
    }
}
