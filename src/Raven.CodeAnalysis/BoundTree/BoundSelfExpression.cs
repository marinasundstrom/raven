namespace Raven.CodeAnalysis;

internal partial class BoundSelfExpression : BoundExpression
{
    public BoundSelfExpression(TypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None) : base(type, type, reason)
    {

    }
}
