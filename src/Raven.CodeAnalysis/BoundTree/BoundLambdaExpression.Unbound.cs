namespace Raven.CodeAnalysis;

internal partial class BoundLambdaExpression
{
    public BoundUnboundLambda? Unbound { get; private set; }

    internal void AttachUnbound(BoundUnboundLambda unbound)
    {
        Unbound = unbound;
    }
}
