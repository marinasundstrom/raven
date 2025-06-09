namespace Raven.CodeAnalysis;

internal partial class BoundReturnExpression : BoundExpression
{
    public BoundReturnExpression(ITypeSymbol returnType, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(returnType, null, candidateReason)
    {

    }
}