namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxNodeExtensions
{
    public static TRoot ReplaceToken<TRoot>(this TRoot root, SyntaxToken tokenInList, params IEnumerable<SyntaxToken> newTokens)
        where TRoot : SyntaxNode
    {
        return (TRoot)root.ReplaceTokenInListCore(tokenInList, newTokens);
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
}
