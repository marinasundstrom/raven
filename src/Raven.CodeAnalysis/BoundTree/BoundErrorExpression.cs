namespace Raven.CodeAnalysis;

internal class BoundErrorExpression : BoundExpression
{
    public BoundErrorExpression(ITypeSymbol type, ISymbol? symbol = null, BoundExpressionReason candidateReason = BoundExpressionReason.None)
        : base(type, symbol, candidateReason)
    {

    }
}
