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

    [Fact]
    public void SyntaxNode_EqualityAcceptsNullableOperands()
    {
        SyntaxNode? missing = null;
        var block = SyntaxFactory.Block();

        Assert.True(missing == null);
        Assert.False(missing != null);
        Assert.False(missing == block);
        Assert.True(missing != block);
    }

    [Fact]
    public void SyntaxToken_DistinguishesDetachedAndAttachedTokens()
    {
        var detachedToken = SyntaxFactory.Identifier("value");
        var defaultToken = default(SyntaxToken);

        Assert.Null(detachedToken.Parent);
        Assert.Null(detachedToken.SyntaxTree);
        Assert.Null(defaultToken.Parent);
        Assert.Null(defaultToken.SyntaxTree);

        var tree = SyntaxTree.ParseText("func Main() {}");
        var token = tree.GetRoot().GetFirstToken();

        Assert.NotNull(token.Parent);
        Assert.Same(tree, token.SyntaxTree);
    }

    [Fact]
    public void SyntaxNodeOrToken_PreservesDefaultAndDetachedOwnership()
    {
        var empty = default(SyntaxNodeOrToken);
        var detachedNode = new SyntaxNodeOrToken(SyntaxFactory.Block());
        var detachedToken = new SyntaxNodeOrToken(SyntaxFactory.Identifier("value"));

        Assert.False(empty.IsNode);
        Assert.False(empty.IsToken);
        Assert.Null(empty.Parent);
        Assert.Null(empty.AsNode());
        Assert.Equal(default, empty.AsToken());

        Assert.True(detachedNode.IsNode);
        Assert.Null(detachedNode.Parent);
        Assert.True(detachedToken.IsToken);
        Assert.Null(detachedToken.Parent);
    }

}
