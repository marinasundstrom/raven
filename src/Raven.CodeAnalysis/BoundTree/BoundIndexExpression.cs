using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundIndexExpression : BoundExpression
{
    public BoundIndexExpression(BoundExpression value, bool isFromEnd, ITypeSymbol type)
        : base(type, null, BoundExpressionReason.None)
    {
        Value = value;
        IsFromEnd = isFromEnd;
    }

    public BoundExpression Value { get; }

    public bool IsFromEnd { get; }
}
