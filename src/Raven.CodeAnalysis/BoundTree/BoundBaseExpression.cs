namespace Raven.CodeAnalysis;

internal partial class BoundBaseExpression : BoundExpression
{
    public BoundBaseExpression(ITypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, type, reason)
    {
    }
}
