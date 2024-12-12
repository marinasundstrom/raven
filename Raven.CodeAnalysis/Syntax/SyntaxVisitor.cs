
namespace Raven.CodeAnalysis.Syntax;

public abstract partial class SyntaxVisitor
{
    public virtual void Visit(SyntaxNode node)
    {
        if (node != null)
        {
            ((SyntaxNode)node).Accept(this);
        }
    }

    public virtual void DefaultVisit(SyntaxNode node)
    {

    }
}
