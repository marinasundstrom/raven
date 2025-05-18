namespace Raven.CodeAnalysis;

public abstract partial class SymbolVisitor
{
    public virtual void Visit(ISymbol symbol)
    {
        if (symbol != null)
        {
            symbol.Accept(this);
        }
    }

    public virtual void DefaultVisit(ISymbol symbol)
    {

    }
}