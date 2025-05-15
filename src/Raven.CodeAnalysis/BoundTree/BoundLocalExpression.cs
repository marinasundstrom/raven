namespace Raven.CodeAnalysis;

internal class BoundLocalExpression : BoundExpression
{
    public BoundLocalExpression(ILocalSymbol local, CandidateReason candidateReason = CandidateReason.None) : base(local.Type, local, candidateReason)
    {

    }
}
