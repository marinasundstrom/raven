namespace Raven.CodeAnalysis;

internal partial class BoundCastExpression : BoundExpression
{
    public BoundExpression Expression { get; }
    public Conversion Conversion { get; }

    public BoundCastExpression(BoundExpression expression, ITypeSymbol type, Conversion conversion)
        : base(type)
    {
        Expression = expression;
        Conversion = conversion;
    }

}
