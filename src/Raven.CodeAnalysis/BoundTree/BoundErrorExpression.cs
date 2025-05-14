namespace Raven.CodeAnalysis;

internal class BoundErrorExpression : BoundExpression
{
    public BoundErrorExpression(ITypeSymbol type, ISymbol? symbol = null, CandidateReason candidateReason = CandidateReason.None)
        : base(type, symbol, candidateReason)
    {

    }
}
