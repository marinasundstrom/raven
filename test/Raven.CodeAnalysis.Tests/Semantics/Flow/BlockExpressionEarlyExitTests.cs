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
    public void IfExpression_InitializerWithReturnStatements_ReportsDiagnostics_AndLeavesErrorType()
    {
        const string code = """
class Foo {
    func Test(flag: bool) {
        val x = if flag {
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
                new DiagnosticResult("RAV1900").WithSpan(4, 13, 4, 22),
                new DiagnosticResult("RAV1900").WithSpan(6, 13, 6, 22),
                new DiagnosticResult("RAV1503").WithAnySpan().WithArguments("int", "()")
            ]);

        var result = verifier.GetResult();
        var tree = result.Compilation.SyntaxTrees.Single();
        var model = result.Compilation.GetSemanticModel(tree);
        var variable = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single(v => v.Identifier.Text == "x");
        var local = (ILocalSymbol)model.GetDeclaredSymbol(variable)!;
        Assert.Equal(SpecialType.None, local.Type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void IfExpression_GlobalInitializerWithReturnStatements_ReportsDiagnostics_AndLeavesErrorType()
    {
        const string code = """
val x = if true {
    return 42
} else {
    return ()
}
""";

        var verifier = CreateVerifier(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult("RAV1900").WithSpan(2, 5, 2, 14),
                new DiagnosticResult("RAV1900").WithSpan(4, 5, 4, 14),
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
        Assert.Equal(SpecialType.None, type.SpecialType);

        verifier.Verify();
    }

    [Fact]
    public void LocalInitializer_BlockExpressionWithReturnStatement_IsAllowed()
    {
        const string code = """
class C {
    func M(f: bool) -> bool {
        val x = {
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
        val foo = obj ?? {
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
    public void MatchExpression_BlockArmWithReturnStatement_ReportsDiagnostic()
    {
        const string code = """
class C {
    func M(value: int) -> int {
        val x = value match {
            0 => {
                return 1
            }
            _ => 2
        }

        return x
    }
}
""";

        var verifier = CreateVerifier(
            code,
            [new DiagnosticResult("RAV1900").WithAnySpan()]);

        verifier.Verify();
    }
}
