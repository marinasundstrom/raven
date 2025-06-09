namespace Raven.CodeAnalysis;

internal partial class BoundLocalFunctionExpression : BoundExpression
{
    public BoundLocalFunctionExpression(IMethodSymbol methodSymbol, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(methodSymbol.ReturnType, methodSymbol, candidateReason)
    {

    }
}