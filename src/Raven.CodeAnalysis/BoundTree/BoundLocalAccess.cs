namespace Raven.CodeAnalysis;

internal partial class BoundLocalAccess : BoundExpression
{
    public BoundLocalAccess(ILocalSymbol local, BoundExpressionReason candidateReason = BoundExpressionReason.None) : base(local.Type, local, candidateReason)
    {
        Local = local;
    }

    public ILocalSymbol Local { get; }
}