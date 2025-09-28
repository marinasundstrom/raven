using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ImperativeContextTests : CompilationTestBase
{
    [Fact]
    public void IfStatement_BindsAsStatement()
    {
        var code = """
class C {
    Test(flag: bool) {
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
    Test(flag: bool) {
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
    Test(flag: bool) {
        if flag return else return
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
    public void WhileStatement_BindsAsStatement()
    {
        var code = """
class C {
    Test(flag: bool) {
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
    public void ForStatement_BindsAsStatement()
    {
        var code = """
class C {
    Test(items: int[]) {
        for each item in items {
            ()
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var forStmt = tree.GetRoot().DescendantNodes().OfType<ForStatementSyntax>().First();
        var bound = model.GetBoundNode(forStmt);

        Assert.IsType<BoundForStatement>(bound);
    }

    [Fact]
    public void BlockExpression_InExpressionStatement_BindsAsStatement()
    {
        var code = """
class C {
    Test() {
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
