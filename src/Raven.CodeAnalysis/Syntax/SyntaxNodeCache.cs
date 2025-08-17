using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxNodeCache
{
    private static readonly ConditionalWeakTable<GreenNode, SyntaxNode> cache =
        new ConditionalWeakTable<GreenNode, SyntaxNode>();

    public static IDictionary<GreenNode, SyntaxNode> Cache => cache.ToDictionary(x => x.Key, x => x.Value);

    public static SyntaxNode GetValue(GreenNode node, Func<GreenNode, SyntaxNode> f)
    {
        return f(node);

        // Disabled cache
        /*
        return cache.GetValue(node, (s) =>
        {
            return f(s);
        });
        */
    }

}