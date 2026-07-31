using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class SyntaxRewriter : SyntaxVisitor<SyntaxNode?>
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

    // Tokens and trivia are GreenNode siblings of SyntaxNode. Rewriters return
    // the concrete green value, so these intentionally replace rather than
    // override the result-visitor hooks inherited with TResult = SyntaxNode?.
    public new virtual SyntaxToken VisitToken(SyntaxToken token)
    {
        return token;
    }

    public new virtual SyntaxTrivia VisitTrivia(SyntaxTrivia trivia)
    {
        return default;
    }
    public virtual SyntaxList? VisitList(SyntaxList list)
    {
        List<GreenNode> newList = [];

        foreach (var item in list.GetChildren())
        {
            newList.Add(item.Accept(this));
        }
        return SyntaxFactory.List(newList);
    }
}
