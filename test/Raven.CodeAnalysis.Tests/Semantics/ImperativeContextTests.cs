using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ImperativeContextTests
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
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

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
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = (BoundIfStatement)model.GetBoundNode(ifStmt);

        Assert.IsType<BoundBlockExpression>(bound.ThenNode);
        Assert.IsType<BoundUnitExpression>(bound.ElseNode);
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
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var ifStmt = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().First();
        var bound = (BoundIfStatement)model.GetBoundNode(ifStmt);

        Assert.IsType<BoundReturnStatement>(bound.ThenNode);
        Assert.IsType<BoundReturnStatement>(bound.ElseNode);
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
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var model = compilation.GetSemanticModel(tree);
        var exprStmt = tree.GetRoot().DescendantNodes().OfType<ExpressionStatementSyntax>()
            .First(es => es.Expression is BlockSyntax);
        var bound = model.GetBoundNode(exprStmt);

        Assert.IsType<BoundBlockStatement>(bound);
    }
}
