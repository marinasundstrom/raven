namespace Raven.CodeAnalysis;

internal sealed partial class BoundReturnExpression : BoundExpression
{
    public BoundReturnExpression(BoundExpression? expression, TypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        Expression = expression;
    }

    public BoundExpression? Expression { get; }
}
