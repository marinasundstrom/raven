using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticModelMappingTests : CompilationTestBase
{
    [Fact]
    public void GetSyntax_ReturnsSyntaxForBoundExpression()
    {
        var (compilation, tree) = CreateCompilation("func Main() { let x = 1 + 2; }");
        var model = compilation.GetSemanticModel(tree);

        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var boundDeclarator = model.GetBoundNode(declarator);

        var syntax = model.GetSyntax(boundDeclarator);

        Assert.NotNull(syntax);
        Assert.Same(declarator.SyntaxTree, syntax!.SyntaxTree);
        Assert.Equal(declarator.Span, syntax.Span);
    }

    [Fact]
    public void GetSyntax_ReturnsSyntaxForBoundStatement()
    {
        var (compilation, tree) = CreateCompilation("func Main() { if true { let x = 0; } }");
        var model = compilation.GetSemanticModel(tree);

        var ifStatement = tree.GetRoot().DescendantNodes().OfType<IfStatementSyntax>().Single();
        var boundIf = model.GetBoundNode(ifStatement);

        var syntax = model.GetSyntax(boundIf);

        Assert.NotNull(syntax);
        Assert.Same(ifStatement.SyntaxTree, syntax!.SyntaxTree);
        Assert.Equal(ifStatement.Span, syntax.Span);
    }
}
