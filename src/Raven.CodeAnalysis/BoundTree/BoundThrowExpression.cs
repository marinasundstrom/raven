namespace Raven.CodeAnalysis;

internal sealed partial class BoundThrowExpression : BoundExpression
{
    public BoundThrowExpression(BoundExpression expression, TypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        Expression = expression;
    }

    public BoundExpression Expression { get; }
}
