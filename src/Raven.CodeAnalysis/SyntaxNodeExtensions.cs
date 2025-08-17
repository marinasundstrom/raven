using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class SyntaxNodeExtensions
{
    public static T? GetAncestor<T>(this SyntaxToken token) where T : SyntaxNode
    {
        var node = token.Parent;
        return node?.GetAncestor<T>();
    }

    public static T? GetAncestor<T>(this SyntaxNode? node) where T : SyntaxNode
    {
        while (node is not null)
        {
            if (node is T match)
                return match;
            node = node.Parent;
        }
        return null;
    }
}