namespace Raven.CodeAnalysis;

internal partial class BoundNullCoalesceExpression : BoundExpression
{
    public BoundExpression Left { get; }
    public BoundExpression Right { get; }

    public BoundNullCoalesceExpression(BoundExpression left, BoundExpression right, ITypeSymbol type)
        : base(type, symbol: null, BoundExpressionReason.None)
    {
        Left = left;
        Right = right;
    }

    public override string ToString() =>
        $"({Left} ?? {Right})";
}
