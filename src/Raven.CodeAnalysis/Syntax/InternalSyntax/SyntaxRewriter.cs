namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class SyntaxRewriter : SyntaxVisitor<SyntaxNode?>
{
    private int _recursionDepth;

    public override SyntaxNode? Visit(SyntaxNode? node)
    {
        if (node is not null)
        {
            _recursionDepth++;

            var result = node.Accept(this);

            _recursionDepth--;

            return result;
        }

        return null;
    }

    public override SyntaxNode? DefaultVisit(SyntaxNode node)
    {
        return node;
    }

    // Tokens and trivia are GreenNode siblings of SyntaxNode. Rewriters return
    // the concrete green value, so these intentionally replace rather than
    // override the result-visitor hooks inherited with TResult = SyntaxNode?.
    public new virtual SyntaxToken VisitToken(SyntaxToken token)
    {
        return token;
    }

    public new virtual SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        return trivia;
    }
    public virtual SyntaxList? VisitList(SyntaxList list)
    {
        List<GreenNode?>? rewritten = null;
        for (var i = 0; i < list.SlotCount; i++)
        {
            var item = list[i];
            GreenNode? visited = item switch
            {
                SyntaxNode node => Visit(node),
                SyntaxToken token => VisitToken(token),
                SyntaxTrivia trivia => VisitTrivia(trivia),
                SyntaxList nested => VisitList(nested),
                _ => item
            };
            if (rewritten is null && !ReferenceEquals(item, visited))
            {
                rewritten = new List<GreenNode?>(list.SlotCount);
                for (var previous = 0; previous < i; previous++)
                    rewritten.Add(list[previous]);
            }
            if (rewritten is not null && visited is not null)
                rewritten.Add(visited);
        }
        return rewritten is null ? list : (SyntaxList)list.With(rewritten.ToArray());
    }
}
