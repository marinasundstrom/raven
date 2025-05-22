namespace Raven.CodeAnalysis;

internal class BoundParameterExpression : BoundExpression
{
    public BoundParameterExpression(IParameterSymbol parameter, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(parameter.Type, parameter, candidateReason)
    {

    }
}