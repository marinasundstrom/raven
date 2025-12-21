using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundRangeExpression : BoundExpression
{
    public BoundRangeExpression(
        BoundIndexExpression? left,
        BoundIndexExpression? right,
        ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
        Left = left;
        Right = right;
    }

    public BoundIndexExpression? Left { get; }

    public BoundIndexExpression? Right { get; }
}
