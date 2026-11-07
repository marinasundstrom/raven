namespace Raven.CodeAnalysis;

internal sealed partial class BoundSelfExpression : BoundExpression
{
    public BoundSelfExpression(TypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None) : base(type, type, reason)
    {

    }
}
