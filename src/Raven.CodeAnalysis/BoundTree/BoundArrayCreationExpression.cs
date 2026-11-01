namespace Raven.CodeAnalysis;

internal sealed partial class BoundArrayCreationExpression : BoundExpression
{
    public BoundArrayCreationExpression(IArrayTypeSymbol arrayType, BoundExpression length)
        : base(arrayType)
    {
        ArrayType = arrayType;
        Length = length;
    }

    public IArrayTypeSymbol ArrayType { get; }
    public BoundExpression Length { get; }
}
