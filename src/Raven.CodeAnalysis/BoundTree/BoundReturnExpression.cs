namespace Raven.CodeAnalysis;

internal partial class BoundReturnExpression : BoundExpression
{
    public BoundReturnExpression(BoundExpression? expression, ITypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        Expression = expression;
    }

    public BoundExpression? Expression { get; }
}
