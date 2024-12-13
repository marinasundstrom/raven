namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxNodeExtensions
{
    /*
    public static TRoot ReplaceToken<TRoot>(this TRoot root, SyntaxToken tokenInList,
        params IEnumerable<SyntaxToken> newTokens)
        where TRoot : SyntaxNode
    {
        return default!; // (TRoot)root.ReplaceTokenInListCore(tokenInList, newTokens);
    }
    */

    public static TSyntax WithLeadingTrivia<TSyntax>(this TSyntax node, params IEnumerable<SyntaxTrivia> trivia)
        where TSyntax : SyntaxNode
    {
        var first = node.GetFirstToken(includeZeroWidth: true);
        var newFirst = first.WithLeadingTrivia(trivia);
        return (TSyntax)node.ReplaceToken(first, newFirst);
    }

    public static TSyntax WithTrailingTrivia<TSyntax>(this TSyntax node, params IEnumerable<SyntaxTrivia> trivia)
        where TSyntax : SyntaxNode
    {
        var last = node.GetLastToken(includeZeroWidth: true);
        var newLast = last.WithTrailingTrivia(trivia);
        return (TSyntax)node.ReplaceToken(last, newLast);
    }
    
    public static TSyntax NormalizeWhitespace<TSyntax>(this TSyntax node)
        where TSyntax : SyntaxNode
    {
        return new SyntaxNormalizer().Visit(node);
    }
}