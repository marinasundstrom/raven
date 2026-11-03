namespace Raven.CodeAnalysis;

internal partial class BoundArrayAccessExpression : BoundExpression
{
    public BoundArrayAccessExpression(BoundExpression receiver, IEnumerable<BoundExpression> indices, TypeSymbol elementType)
        : base(elementType, null, BoundExpressionReason.None)
    {
        Receiver = receiver;
        Indices = indices;
        ElementType = elementType;
    }

    public BoundExpression Receiver { get; }
    public IEnumerable<BoundExpression> Indices { get; }
    public TypeSymbol ElementType { get; }
}
