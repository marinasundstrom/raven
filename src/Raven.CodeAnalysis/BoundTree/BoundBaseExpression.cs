namespace Raven.CodeAnalysis;

internal partial class BoundBaseExpression : BoundExpression
{
    public BoundExpression? Receiver { get; }

    public BoundBaseExpression(ITypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None, BoundExpression? receiver = null)
        : base(type, type, reason)
    {
        Receiver = receiver;
    }
}
