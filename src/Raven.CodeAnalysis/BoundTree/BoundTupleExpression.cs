namespace Raven.CodeAnalysis;

internal partial class BoundTupleExpression : BoundExpression
{
    public BoundTupleExpression(IEnumerable<BoundExpression> elements, ITypeSymbol type, IEnumerable<string> elementNames)
            : base(type, null, BoundExpressionReason.None)

    {
        Elements = elements;
        Type = type;
        ElementNames = elementNames;
    }

    public IEnumerable<BoundExpression> Elements { get; }
    public override ITypeSymbol Type { get; }
    public IEnumerable<string> ElementNames { get; }
}