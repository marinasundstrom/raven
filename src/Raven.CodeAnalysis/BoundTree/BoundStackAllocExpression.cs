namespace Raven.CodeAnalysis;

internal sealed partial class BoundStackAllocExpression : BoundExpression
{
    public BoundStackAllocExpression(ITypeSymbol elementType, BoundExpression count, ITypeSymbol pointerType)
        : base(pointerType)
    {
        ElementType = elementType;
        Count = count;
        PointerType = pointerType;
    }

    public ITypeSymbol ElementType { get; }

    public BoundExpression Count { get; }

    public ITypeSymbol PointerType { get; }
}
