

namespace Raven.CodeAnalysis.Syntax;

public abstract partial class SyntaxVisitor
{
    public virtual void Visit(SyntaxNode node)
    {
        if (node is not null)
        {
            node.Accept(this);
        }
    }

    public virtual void DefaultVisit(SyntaxNode node)
    {

    }

    public virtual void VisitTrivia(SyntaxTrivia syntaxTrivia)
    {

    }
}