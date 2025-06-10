namespace Raven.CodeAnalysis;

internal partial class BoundPropertyAccess : BoundExpression
{
    public BoundPropertyAccess(IPropertySymbol property, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(property.Type, property, candidateReason)
    {
        Property = property;
    }

    public IPropertySymbol Property { get; }
}