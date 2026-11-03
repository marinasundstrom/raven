namespace Raven.CodeAnalysis;

internal sealed partial class BoundArrayCreationExpression : BoundExpression
{
    public BoundArrayCreationExpression(ArrayTypeSymbol arrayType, BoundExpression length)
        : base(arrayType)
    {
        ArrayType = arrayType;
        Length = length;
    }

    public ArrayTypeSymbol ArrayType { get; }
    public BoundExpression Length { get; }
}
