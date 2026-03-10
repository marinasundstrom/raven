using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundRangeExpression : BoundExpression
{
    public BoundRangeExpression(
        BoundIndexExpression? left,
        BoundIndexExpression? right,
        ITypeSymbol type,
        bool isUpperExclusive = false)
        : base(type, null, BoundExpressionReason.None)
    {
        Left = left;
        Right = right;
        IsUpperExclusive = isUpperExclusive;
    }

    public BoundIndexExpression? Left { get; }

    public BoundIndexExpression? Right { get; }

    public bool IsUpperExclusive { get; }
}
