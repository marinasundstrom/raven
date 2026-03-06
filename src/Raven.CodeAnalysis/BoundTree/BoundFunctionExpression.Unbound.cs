namespace Raven.CodeAnalysis;

internal partial class BoundFunctionExpression
{
    public BoundUnboundFunctionExpression? Unbound { get; private set; }

    internal void AttachUnbound(BoundUnboundFunctionExpression unbound)
    {
        Unbound = unbound;
    }
}
