namespace Raven.CodeAnalysis;

internal partial class BoundTypeOfExpression : BoundExpression
{
    public BoundTypeOfExpression(ITypeSymbol operandType, ITypeSymbol systemType)
        : base(systemType)
    {
        OperandType = operandType;
    }

    public ITypeSymbol OperandType { get; }
}
