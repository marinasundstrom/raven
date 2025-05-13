namespace Raven.CodeAnalysis;

class BoundLiteralExpression : BoundExpression
{
    public object Value { get; }
    public override ITypeSymbol Type { get; }

    public BoundLiteralExpression(object value, ITypeSymbol type)
    {
        Value = value;
        Type = type;
    }
}
