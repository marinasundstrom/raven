namespace Raven.CodeAnalysis.Syntax;

internal sealed class SyntaxReplacer : SyntaxRewriter
{
    private readonly IReadOnlyDictionary<SyntaxNode, SyntaxNode?>? _nodeMap;
    private readonly IReadOnlyDictionary<SyntaxToken, SyntaxToken?>? _tokenMap;
    private readonly IReadOnlyDictionary<SyntaxTrivia, SyntaxTrivia?>? _triviaMap;

    public SyntaxReplacer(
        IReadOnlyDictionary<SyntaxNode, SyntaxNode?>? nodeMap,
        IReadOnlyDictionary<SyntaxToken, SyntaxToken?>? tokenMap,
        IReadOnlyDictionary<SyntaxTrivia, SyntaxTrivia?>? triviaMap)
    {
        _nodeMap = nodeMap;
        _tokenMap = tokenMap;
        _triviaMap = triviaMap;
    }

    public override SyntaxNode Visit(SyntaxNode node)
    {
        if (node == null)
            return null!;

        if (_nodeMap != null && _nodeMap.TryGetValue(node, out var replacement))
        {
            return replacement ?? node;
        }

        return base.Visit(node);
    }

    public override SyntaxToken VisitToken(SyntaxToken token)
    {
        if (_tokenMap != null && _tokenMap.TryGetValue(token, out var replacement))
        {
            return replacement ?? token;
        }

        var leading = VisitList(token.LeadingTrivia);
        var trailing = VisitList(token.TrailingTrivia);

        if (leading != token.LeadingTrivia || trailing != token.TrailingTrivia)
        {
            token = token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing);
        }

        return token;
    }

    public override SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        if (_triviaMap != null && _triviaMap.TryGetValue(trivia, out var replacement))
        {
            return replacement ?? trivia;
        }

        if (trivia.HasStructure)
        {
            var structure = trivia.GetStructure();
            var newStructure = Visit(structure);

            if (structure != newStructure)
                return SyntaxFactory.Trivia((StructuredTriviaSyntax)newStructure);
        }

        return trivia;
    }
}