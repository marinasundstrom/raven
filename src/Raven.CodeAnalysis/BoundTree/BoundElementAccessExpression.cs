namespace Raven.CodeAnalysis;

internal partial class BoundArrayAccessExpression : BoundExpression
{
    public BoundArrayAccessExpression(BoundExpression receiver, BoundExpression[] indices, ITypeSymbol elementType)
        : base(elementType, null, BoundExpressionReason.None)
    {
        Receiver = receiver;
        Indices = indices;
        ElementType = elementType;
    }

    public BoundExpression Receiver { get; }
    public BoundExpression[] Indices { get; }
    public ITypeSymbol ElementType { get; }
}