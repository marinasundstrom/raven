namespace Raven.CodeAnalysis;

internal sealed partial class BoundLocalAccess : BoundExpression
{
    public BoundLocalAccess(LocalSymbol local, BoundExpressionReason reason = BoundExpressionReason.None) : base(local.Type, local, reason)
    {
        Local = local;
    }

    public LocalSymbol Local { get; }
}
