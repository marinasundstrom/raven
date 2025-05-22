namespace Raven.CodeAnalysis;

internal class BoundLocalExpression : BoundExpression
{
    public BoundLocalExpression(ILocalSymbol local, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(local.Type, local, candidateReason)
    {

    }
}