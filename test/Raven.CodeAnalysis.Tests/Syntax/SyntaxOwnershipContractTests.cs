namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SyntaxOwnershipContractTests
{
    [Fact]
    public void SyntaxNode_DistinguishesDetachedRootAndAttachedChildren()
    {
        var detachedBlock = SyntaxFactory.Block();

        Assert.Null(detachedBlock.Parent);
        Assert.Null(detachedBlock.SyntaxTree);

        var tree = SyntaxTree.ParseText("func Main() {}");
        var root = tree.GetRoot();
        var global = Assert.IsType<GlobalStatementSyntax>(Assert.Single(root.Members));
        var function = Assert.IsType<FunctionStatementSyntax>(global.Statement);

        Assert.Null(root.Parent);
        Assert.Same(tree, root.SyntaxTree);
        Assert.Same(root, global.Parent);
        Assert.Same(global, function.Parent);
        Assert.Same(tree, function.SyntaxTree);
    }

}
