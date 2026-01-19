

namespace Raven.CodeAnalysis;

abstract class BoundExpression : BoundNode
{
    public virtual ITypeSymbol Type { get; }
    public virtual ISymbol? Symbol { get; }
    public virtual BoundExpressionReason Reason { get; }

    protected BoundExpression(ITypeSymbol type, ISymbol? symbol = null, BoundExpressionReason reason = BoundExpressionReason.None)
    {
        Type = type;
        Symbol = symbol;
        Reason = reason;
    }

    public virtual ITypeSymbol? GetConvertedType() => Type; // post-conversion

    public virtual ITypeSymbol? GetNaturalType() => Type;   // pre-context
}
