namespace Raven.CodeAnalysis;

internal partial class BoundCollectionExpression : BoundExpression
{
    public BoundCollectionExpression(
        ITypeSymbol type,
        IEnumerable<BoundExpression> elements,
        ISymbol? collectionSymbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, type, reason)
    {
        Elements = elements;
        CollectionSymbol = collectionSymbol;
    }

    public IEnumerable<BoundExpression> Elements { get; }

    public ISymbol? CollectionSymbol { get; }
}

internal partial class BoundEmptyCollectionExpression : BoundExpression
{
    public BoundEmptyCollectionExpression(
        ITypeSymbol? type = null,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type!, null, reason)
    {

    }
}