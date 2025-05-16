namespace Raven.CodeAnalysis;

internal class BoundPropertyExpression : BoundExpression
{
    public BoundPropertyExpression(IPropertySymbol property, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(property.Type, property, candidateReason)
    {

    }
}