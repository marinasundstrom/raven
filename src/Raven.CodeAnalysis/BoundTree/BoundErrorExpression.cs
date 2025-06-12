namespace Raven.CodeAnalysis;

internal partial class BoundErrorExpression : BoundExpression
{
    public BoundErrorExpression(ITypeSymbol type, ISymbol? symbol = null, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, symbol, reason)
    {

    }
}