namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxNodeExtensions
{
    public static TRoot ReplaceNode<TRoot>(this TRoot node, SyntaxNode oldNode, SyntaxNode newNode)
          where TRoot : SyntaxNode
    {
        return (TRoot)node.ReplaceNodeCore(oldNode, newNode);
    }

    public static TRoot ReplaceToken<TRoot>(this TRoot node, SyntaxToken oldToken, SyntaxToken newToken)
        where TRoot : SyntaxNode
    {
        return (TRoot)node.ReplaceTokenCore(oldToken, newToken);
    }

    public static TRoot ReplaceNode<TRoot>(this TRoot node, SyntaxNode oldNode, IEnumerable<SyntaxNode> newNodes)
        where TRoot : SyntaxNode
    {
        return (TRoot)node.ReplaceNodeWithNodesCore(oldNode, newNodes);
    }

    public static TSyntax WithLeadingTrivia<TSyntax>(this TSyntax node, params IEnumerable<SyntaxTrivia> trivia)
        where TSyntax : SyntaxNode
    {
        var first = node.GetFirstToken(includeZeroWidth: true);
        var newFirst = first.WithLeadingTrivia(trivia);
        return node.ReplaceToken(first, newFirst);
    }

    public static TSyntax WithTrailingTrivia<TSyntax>(this TSyntax node, params IEnumerable<SyntaxTrivia> trivia)
        where TSyntax : SyntaxNode
    {
        var last = node.GetLastToken(includeZeroWidth: true);
        var newLast = last.WithTrailingTrivia(trivia);
        return node.ReplaceToken(last, newLast);
    }

    public static TSyntax NormalizeWhitespace<TSyntax>(this TSyntax node)
        where TSyntax : SyntaxNode
    {
        return new SyntaxNormalizer().Visit(node);
    }

    public static IEnumerable<SyntaxNode> AncestorNodesAndSelf(this SyntaxNode node)
    {
        yield return node;

        foreach (var n in node.Ancestors())
        {
            yield return n;
        }
    }

    public static IEnumerable<SyntaxNode> DescendantNodesAndSelf(this SyntaxNode node)
    {
        yield return node;

        foreach (var n in node.DescendantNodes())
        {
            yield return n;
        }
    }
}