namespace Raven.CodeAnalysis;

internal sealed partial class BoundStackAllocExpression : BoundExpression
{
    public BoundStackAllocExpression(ITypeSymbol elementType, BoundExpression count, ITypeSymbol allocationType)
        : base(allocationType)
    {
        ElementType = elementType;
        Count = count;
        AllocationType = allocationType;
    }

    public ITypeSymbol ElementType { get; }

    public BoundExpression Count { get; }

    public ITypeSymbol AllocationType { get; }
}
