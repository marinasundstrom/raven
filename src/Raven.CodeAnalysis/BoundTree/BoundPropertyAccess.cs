namespace Raven.CodeAnalysis;

internal partial class BoundPropertyAccess : BoundExpression
{
    public BoundPropertyAccess(IPropertySymbol property, BoundExpressionReason reason = BoundExpressionReason.None) : base(property.Type, property, reason)
    {
        Property = property;
    }

    public IPropertySymbol Property { get; }
}