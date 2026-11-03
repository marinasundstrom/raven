namespace Raven.CodeAnalysis;

internal partial class BoundAsExpression : BoundExpression
{
    public BoundExpression Expression { get; }
    public Conversion Conversion { get; }

    public BoundAsExpression(BoundExpression expression, TypeSymbol type, Conversion conversion)
        : base(type)
    {
        Expression = expression;
        Conversion = conversion;
    }
}
