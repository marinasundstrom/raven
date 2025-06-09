namespace Raven.CodeAnalysis;

internal partial class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }

    public BoundLiteralExpression(object value, ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
        Value = value;
    }
}