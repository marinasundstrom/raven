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
    public void EquivalentDetachedSyntaxNodes_CompareWithoutThrowing()
    {
        var block = SyntaxFactory.Block();
        var equivalent = block.WithParent(parent: null, position: 0);

        Assert.NotSame(block, equivalent);
        Assert.Equal(block, equivalent);
        Assert.True(block == equivalent);
    }

    [Fact]
    public void DetachedSyntaxNode_HasNoSourceLocationOrReference()
    {
        var block = SyntaxFactory.Block();

        Assert.Same(Location.None, block.GetLocation());
        var exception = Assert.Throws<InvalidOperationException>(() => block.GetReference());
        Assert.Contains("detached syntax node", exception.Message, StringComparison.OrdinalIgnoreCase);

        var tree = SyntaxTree.ParseText("func Main() {}");
        var root = tree.GetRoot();
        var reference = root.GetReference();

        Assert.Same(tree, reference.SyntaxTree);
        Assert.Equal(root.Span, reference.Span);
    }

    [Fact]
    public void ReplaceSyntax_NullRootReplacementPreservesTheRootNode()
    {
        var block = SyntaxFactory.Block();
        var replacements = new Dictionary<SyntaxNode, SyntaxNode?>
        {
            [block] = null,
        };

        var result = block.ReplaceSyntax(replacements, tokenMap: null, triviaMap: null);

        Assert.Same(block, result);
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
    public void DefaultSyntaxToken_IsSafeToInspectAndTransform()
    {
        var token = default(SyntaxToken);

        Assert.Equal(SyntaxKind.None, token.Kind);
        Assert.Equal(default, token.Span);
        Assert.Equal(default, token.FullSpan);
        Assert.Empty(token.GetAnnotations(["test"]));
        Assert.Null(token.GetAnnotation("test"));
        Assert.False(token.HasAnnotation(new SyntaxAnnotation("test")));
        Assert.Equal(token, token.WithLeadingTrivia(SyntaxTriviaList.Empty));
        Assert.Equal(token, token.WithTrailingTrivia(SyntaxTriviaList.Empty));
        Assert.Equal(token, token.WithAdditionalAnnotations(new SyntaxAnnotation("test")));
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
