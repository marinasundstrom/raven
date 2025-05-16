namespace Raven.CodeAnalysis;

sealed class BoundCollectionExpression : BoundExpression
{
    public BoundCollectionExpression(
        ITypeSymbol type,
        BoundExpression[] elements,
        ISymbol? collectionSymbol = null,
        BoundExpressionReason candidateReason = BoundExpressionReason.None)
        : base(type, type, candidateReason)
    {
        Elements = elements;
    }

    public BoundExpression[] Elements { get; }
}

sealed class BoundEmptyCollectionExpression : BoundExpression
{
    public BoundEmptyCollectionExpression(
        ITypeSymbol? type = null,
        BoundExpressionReason candidateReason = BoundExpressionReason.None)
        : base(type!, null, candidateReason)
    {

    }
}