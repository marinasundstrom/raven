namespace Raven.CodeAnalysis;

internal class BoundReturnExpression : BoundExpression
{
    public BoundReturnExpression(ITypeSymbol returnType, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(returnType, null, candidateReason)
    {

    }
}