namespace Raven.CodeAnalysis;

internal partial class BoundLocalAccess : BoundExpression
{
    public BoundLocalAccess(ILocalSymbol local, BoundExpressionReason reason = BoundExpressionReason.None) : base(local.Type, local, reason)
    {
        Local = local;
    }

    public ILocalSymbol Local { get; }
}