using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class StatementBindingSemanticTests : CompilationTestBase
{
    [Fact]
    public void NestedStatementBlockExpressionStatement_DoesNotRequireMethodReturnType()
    {
        var code = """
class C {
    func Test(flag: bool, action: () -> ()) -> int {
        if flag {
            action()
        }
        return 1
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Id == "RAV1503");
    }

    [Fact]
    public void NestedStatementBlockInMethodReturningSequence_DoesNotRequireMethodReturnType()
    {
        var code = """
import System.Collections.Generic.*

class C {
    func Test(flag: bool, list: List<int>) -> IEnumerable<int> {
        if flag {
            list.Add(1)
        }
        return list
    }
}
""";

        var (compilation, _) = CreateCompilation(code);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Id == "RAV1503");
    }

    [Fact]
    public void IfStatement_BindsAsStatement()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        } else {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = model.GetBoundNode(ifStmt);

        Assert.IsType<BoundIfStatement>(bound);
    }

    [Fact]
    public void IfStatement_BranchesCanBeExpressions()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag {
            ()
        } else ()
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = (BoundIfStatement)model.GetBoundNode(ifStmt);

        Assert.IsType<BoundBlockStatement>(bound.ThenNode);
        Assert.IsType<BoundExpressionStatement>(bound.ElseNode);
        Assert.IsType<BoundUnitExpression>(((BoundExpressionStatement)bound.ElseNode!).Expression);
    }

    [Fact]
    public void IfStatement_BranchesCanBeStatements()
    {
        var code = """
class C {
    func Test(flag: bool) {
        if flag
            return
        else
            return
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = (BoundIfStatement)model.GetBoundNode(ifStmt);

        Assert.IsType<BoundReturnStatement>(bound.ThenNode);
        Assert.IsType<BoundReturnStatement>(bound.ElseNode);
    }

    [Fact]
    public void WhileStatement_SingleStatementBody_BindsAsStatement()
    {
        var code = """
class C {
    func Test(flag: bool) {
        while flag
            return
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var whileStmt = tree.GetRoot().DescendantNodes().OfType<WhileStatementSyntax>().First();
        var bound = (BoundWhileStatement)model.GetBoundNode(whileStmt);

        Assert.IsType<BoundReturnStatement>(bound.Body);
    }

    [Fact]
    public void WhileStatement_BindsAsStatement()
    {
        var code = """
class C {
    func Test(flag: bool) {
        while flag {
            break
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var whileStmt = tree.GetRoot().DescendantNodes().OfType<WhileStatementSyntax>().First();
        var bound = model.GetBoundNode(whileStmt);

        Assert.IsType<BoundWhileStatement>(bound);
    }

    [Fact]
    public void BlockExpression_InExpressionStatement_BindsAsStatement()
    {
        var code = """
class C {
    func Test() {
        {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var blockStmt = tree.GetRoot().DescendantNodes().OfType<BlockStatementSyntax>().Skip(1).First();
        var bound = model.GetBoundNode(blockStmt);

        Assert.IsType<BoundBlockStatement>(bound);
    }
}
