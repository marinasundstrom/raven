namespace Raven.CodeAnalysis;

internal partial class BoundConditionalAccessExpression : BoundExpression
{
    public BoundConditionalAccessExpression(BoundExpression receiver, BoundExpression whenNotNull, TypeSymbol type)
        : base(type)
    {
        Receiver = receiver;
        WhenNotNull = whenNotNull;
    }

    public BoundExpression Receiver { get; }
    public BoundExpression WhenNotNull { get; }
}
