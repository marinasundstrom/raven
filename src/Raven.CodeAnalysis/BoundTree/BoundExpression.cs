
namespace Raven.CodeAnalysis;

abstract class BoundExpression : BoundNode
{
    public ITypeSymbol Type { get; }
    public ISymbol? Symbol { get; }
    public BoundExpressionReason Reason { get; }

    protected BoundExpression(ITypeSymbol type, ISymbol? symbol = null, BoundExpressionReason reason = BoundExpressionReason.None)
    {
        Type = type;
        Symbol = symbol;
        Reason = reason;
    }
}
