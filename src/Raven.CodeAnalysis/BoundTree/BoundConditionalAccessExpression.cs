namespace Raven.CodeAnalysis;

internal partial class BoundConditionalAccessExpression : BoundExpression
{
    public BoundConditionalAccessExpression(BoundExpression receiver, BoundExpression whenNotNull, ITypeSymbol type)
        : base(type)
    {
        Receiver = receiver;
        WhenNotNull = whenNotNull;
    }

    public BoundExpression Receiver { get; }
    public BoundExpression WhenNotNull { get; }
}
