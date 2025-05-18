namespace Raven.CodeAnalysis;

public abstract partial class SymbolVisitor
{
    public virtual void Visit(ISymbol node)
    {
        if (node != null)
        {
            node.Accept(this);
        }
    }

    public virtual void DefaultVisit(ISymbol node)
    {

    }
}