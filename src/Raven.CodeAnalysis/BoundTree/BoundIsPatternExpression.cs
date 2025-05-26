namespace Raven.CodeAnalysis;

internal class BoundIsPatternExpression : BoundExpression
{
    public BoundIsPatternExpression(ILocalSymbol local, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(local.Type, local, candidateReason)
    {

    }
}