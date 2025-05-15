namespace Raven.CodeAnalysis;

internal class BoundPropertyExpression : BoundExpression
{
    public BoundPropertyExpression(IPropertySymbol property, CandidateReason candidateReason = CandidateReason.None) : base(property.Type, property, candidateReason)
    {

    }
}