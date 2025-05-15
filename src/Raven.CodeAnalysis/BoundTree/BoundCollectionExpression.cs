namespace Raven.CodeAnalysis;

sealed class BoundCollectionExpression : BoundExpression
{
    public BoundCollectionExpression(
        ITypeSymbol type,
        BoundExpression[] elements,
        ISymbol? collectionSymbol = null,
        CandidateReason candidateReason = CandidateReason.None)
        : base(type, type, candidateReason)
    {
        Elements = elements;
    }

    public BoundExpression[] Elements { get; }
}