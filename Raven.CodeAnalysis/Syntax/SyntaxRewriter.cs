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
        return default!;
    }

    /// <summary>
    /// Temporary
    /// </summary>
    public virtual SyntaxToken? VisitToken(SyntaxToken? token)
    {
        return default;
    }

    public virtual SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        return default;
    }

    public virtual SyntaxNode? VisitStatement(StatementSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxNode? VisitExpression(ExpressionSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxNode? VisitType(TypeSyntax node)
    {
        return node.Accept(this);
    }

    public virtual SyntaxList<TElement>? VisitList<TElement>(SyntaxList<TElement> list)
        where TElement : SyntaxNode
    {
        List<TElement> newList = [];

        foreach (var item in list)
        {
            newList.Add((TElement)item.Accept(this));
        }
        return SyntaxFactory.List<TElement>(list);
    }

    public virtual SeparatedSyntaxList<TElement>? VisitList<TElement>(SeparatedSyntaxList<TElement> list)
        where TElement : SyntaxNode
    {
        List<TElement> newList = [];

        foreach (var item in list)
        {
            newList.Add((TElement)item.Accept(this));
        }
        return SyntaxFactory.SeparatedList<TElement>(list.Select(x => new SyntaxNodeOrToken(x)).ToArray());
    }
}