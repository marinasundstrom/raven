using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public abstract partial class SyntaxRewriter : SyntaxVisitor<SyntaxNode?>
{
    private int _recursionDepth;

    [return: NotNullIfNotNull(nameof(node))]
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

    public virtual SyntaxToken VisitToken(SyntaxToken token)
    {
        return token;
    }

    /// <summary>
    /// Temporary
    /// </summary>
    public virtual SyntaxToken? VisitToken(SyntaxToken? token)
    {
        return default;
    }

    public override SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        return default;
    }
    public virtual SyntaxList<TElement>? VisitList<TElement>(SyntaxList<TElement>? list0)
        where TElement : SyntaxNode
    {
        var list = list0.GetValueOrDefault();

        List<TElement> newList = [];

        foreach (var item in list)
        {
            if (item is null)
                continue;

            var visited = (TElement?)item.Accept(this);
            if (visited is not null)
                newList.Add(visited);
        }
        return SyntaxFactory.List<TElement>(newList);
    }

    public virtual SyntaxList<TElement> VisitList<TElement>(SyntaxList<TElement> list)
        where TElement : SyntaxNode
    {
        List<TElement> newList = [];

        foreach (var item in list)
        {
            if (item is null)
                continue;

            var visited = (TElement?)item.Accept(this);
            if (visited is not null)
                newList.Add(visited);
        }
        return SyntaxFactory.List<TElement>(newList);
    }

    public virtual SyntaxTokenList VisitTokenList(SyntaxTokenList list)
    {
        List<SyntaxToken> newList = [];

        foreach (var item in list)
        {
            newList.Add(item.Accept(this));
        }
        return SyntaxFactory.TokenList(newList);
    }

    public virtual SyntaxTriviaList VisitList(SyntaxTriviaList list)
    {
        List<SyntaxTrivia> newList = [];

        foreach (var item in list)
        {
            newList.Add(item.Accept(this));
        }
        return SyntaxFactory.TriviaList(newList);
    }

    public virtual SeparatedSyntaxList<TElement>? VisitList<TElement>(SeparatedSyntaxList<TElement>? list0)
        where TElement : SyntaxNode
    {
        var list = list0.GetValueOrDefault();

        List<SyntaxNodeOrToken> newList = [];

        foreach (var item in list.GetWithSeparators())
        {
            if (item.TryGetNode(out var node))
            {
                if (node is null)
                    continue;

                var visited = node.Accept(this);
                if (visited is not null)
                    newList.Add(new SyntaxNodeOrToken(visited));
            }
            else if (item.TryGetToken(out var token))
            {
                newList.Add(VisitToken(token));
            }
        }
        return SyntaxFactory.SeparatedList<TElement>(newList.ToArray());
    }

    public virtual SeparatedSyntaxList<TElement> VisitList<TElement>(SeparatedSyntaxList<TElement> list)
    where TElement : SyntaxNode
    {
        List<SyntaxNodeOrToken> newList = [];

        foreach (var item in list.GetWithSeparators())
        {
            if (item.TryGetNode(out var node))
            {
                if (node is null)
                    continue;

                var visited = node.Accept(this);
                if (visited is not null)
                    newList.Add(new SyntaxNodeOrToken(visited));
            }
            else if (item.TryGetToken(out var token))
            {
                newList.Add(VisitToken(token));
            }
        }
        return SyntaxFactory.SeparatedList<TElement>(newList.ToArray());
    }
}
