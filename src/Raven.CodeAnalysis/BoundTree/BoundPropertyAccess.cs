namespace Raven.CodeAnalysis;

internal partial class BoundPropertyAccess : BoundExpression
{
    public BoundPropertyAccess(PropertySymbol property, BoundExpressionReason reason = BoundExpressionReason.None) : base(property.Type, property, reason)
    {
        Property = property;
    }

    public PropertySymbol Property { get; }
}
