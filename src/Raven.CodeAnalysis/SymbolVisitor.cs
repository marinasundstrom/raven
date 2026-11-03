namespace Raven.CodeAnalysis;

public abstract partial class SymbolVisitor
{
    public virtual void Visit(Symbol symbol)
    {
        if (symbol is not null)
        {
            symbol.Accept(this);
        }
    }

    public virtual void DefaultVisit(Symbol symbol)
    {

    }
}
