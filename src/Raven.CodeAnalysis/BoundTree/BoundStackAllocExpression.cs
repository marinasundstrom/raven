namespace Raven.CodeAnalysis;

internal sealed partial class BoundStackAllocExpression : BoundExpression
{
    public BoundStackAllocExpression(TypeSymbol elementType, BoundExpression count, TypeSymbol allocationType)
        : base(allocationType)
    {
        ElementType = elementType;
        Count = count;
        AllocationType = allocationType;
    }

    public TypeSymbol ElementType { get; }

    public BoundExpression Count { get; }

    public TypeSymbol AllocationType { get; }
}
