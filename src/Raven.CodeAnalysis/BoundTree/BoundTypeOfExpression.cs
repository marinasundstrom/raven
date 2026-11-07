namespace Raven.CodeAnalysis;

internal sealed partial class BoundTypeOfExpression : BoundExpression
{
    public BoundTypeOfExpression(TypeSymbol operandType, TypeSymbol systemType)
        : base(systemType)
    {
        OperandType = operandType;
        SystemType = systemType;
    }

    public TypeSymbol OperandType { get; }

    public TypeSymbol SystemType { get; }
}
