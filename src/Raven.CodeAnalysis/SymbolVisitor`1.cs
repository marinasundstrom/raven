namespace Raven.CodeAnalysis;

public abstract partial class SymbolVisitor<TResult>
{
    public virtual TResult? Visit(Symbol? symbol)
    {
        if (symbol is not null)
        {
            return symbol.Accept(this);
        }

        // should not come here too often so we will put this at the end of the method.
        return default;
    }

    public virtual TResult DefaultVisit(Symbol symbol)
    {
        return default!;
    }
}
