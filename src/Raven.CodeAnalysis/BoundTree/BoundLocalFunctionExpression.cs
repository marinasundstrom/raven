namespace Raven.CodeAnalysis;

internal partial class BoundLocalFunctionStatement : BoundStatement
{
    public BoundLocalFunctionStatement(IMethodSymbol methodSymbol, BoundExpressionReason candidateReason = BoundExpressionReason.None)
    {
        Method = methodSymbol;
    }

    public IMethodSymbol Method { get; }
}