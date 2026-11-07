namespace Raven.CodeAnalysis;

internal sealed partial class BoundTupleExpression : BoundExpression
{
    public BoundTupleExpression(IEnumerable<BoundExpression> elements, TypeSymbol type)
            : base(type, null, BoundExpressionReason.None)

    {
        Elements = elements;
        Type = type;
    }

    public IEnumerable<BoundExpression> Elements { get; }
    public override TypeSymbol Type { get; }
}
