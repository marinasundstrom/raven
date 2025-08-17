namespace Raven.CodeAnalysis;

public abstract partial class SymbolVisitor<TResult>
{
    public virtual TResult? Visit(ISymbol? symbol)
    {
        if (symbol is not null)
        {
            return symbol.Accept(this);
        }

        // should not come here too often so we will put this at the end of the method.
        return default;
    }

    public virtual TResult DefaultVisit(ISymbol symbol)
    {
        return default!;
    }
}