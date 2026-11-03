

namespace Raven.CodeAnalysis;

abstract class BoundExpression : BoundNode
{
    public virtual TypeSymbol Type { get; }
    public virtual Symbol? Symbol { get; }
    public virtual BoundExpressionReason Reason { get; }

    protected BoundExpression(TypeSymbol type, Symbol? symbol = null, BoundExpressionReason reason = BoundExpressionReason.None)
    {
        Type = type;
        Symbol = symbol;
        Reason = reason;
    }

    public virtual TypeSymbol? GetConvertedType() => Type; // post-conversion

    public virtual TypeSymbol? GetNaturalType() => Type;   // pre-context

    public BoundRequiredResultExpression RequireValue() => new BoundRequiredResultExpression(this);
}
