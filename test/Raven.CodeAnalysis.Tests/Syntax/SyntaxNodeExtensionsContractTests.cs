#if RAVEN_PORT
extern alias Port;
using NodeExtensionApi = Port::Raven.CodeAnalysis.Syntax.SyntaxNodeExtensions;
#else
using NodeExtensionApi = Raven.CodeAnalysis.Syntax.SyntaxNodeExtensions;
#endif

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Syntax;

[Trait("Target", "Shared")]
public sealed class SyntaxNodeExtensionsContractTests
{
    [Fact]
    public void ReplacementAndTriviaPreserveRootTypeAndOriginalTree()
    {
        var original = SyntaxFactory.IdentifierName("before");
        var replacement = SyntaxFactory.IdentifierName("after");
        var changed = NodeExtensionApi.ReplaceNode(original, original, replacement);
        Assert.IsType<IdentifierNameSyntax>(changed);
        Assert.Equal("after", changed.ToFullString());
        var tokenChanged = NodeExtensionApi.ReplaceToken(original, original.Identifier, SyntaxFactory.Identifier("token"));
        Assert.Equal("token", tokenChanged.ToFullString());
        var leading = NodeExtensionApi.WithLeadingTrivia(original, SyntaxFactory.Whitespace(" "));
        var trailing = NodeExtensionApi.WithTrailingTrivia(leading, SyntaxFactory.Whitespace("  "));
        Assert.Equal(" before  ", trailing.ToFullString());
        Assert.Equal("before", original.ToFullString());
    }

    [Fact]
    public void SelfTraversalsIncludeRootAndKeepTreeOrder()
    {
        var root = SyntaxFactory.ParenthesizedExpression(SyntaxFactory.IdentifierName("value"));
        var child = root.Expression;
        Assert.Equal(new SyntaxNode[] { child, root }, NodeExtensionApi.AncestorNodesAndSelf(child));
        Assert.Equal(new SyntaxNode[] { root, child }, NodeExtensionApi.DescendantNodesAndSelf(root));
    }
}
