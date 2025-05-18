namespace Raven.CodeAnalysis;

public abstract partial class SymbolVisitor<TResult>
{
    public virtual TResult? Visit(ISymbol? node)
    {
        if (node != null)
        {
            return node.Accept(this);
        }

        // should not come here too often so we will put this at the end of the method.
        return default;
    }

    public virtual TResult DefaultVisit(ISymbol node)
    {
        return default!;
    }
}