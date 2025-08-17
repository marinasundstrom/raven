namespace Raven.CodeAnalysis;

internal partial class BoundTupleExpression : BoundExpression
{
    public BoundTupleExpression(IEnumerable<BoundExpression> elements, ITypeSymbol type)
            : base(type, null, BoundExpressionReason.None)

    {
        Elements = elements;
        Type = type;
    }

    public IEnumerable<BoundExpression> Elements { get; }
    public override ITypeSymbol Type { get; }
}