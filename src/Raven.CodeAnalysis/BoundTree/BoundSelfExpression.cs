namespace Raven.CodeAnalysis;

internal partial class BoundSelfExpression : BoundExpression
{
    public BoundSelfExpression(ITypeSymbol type, BoundExpressionReason reason = BoundExpressionReason.None) : base(type, type, reason)
    {

    }
}