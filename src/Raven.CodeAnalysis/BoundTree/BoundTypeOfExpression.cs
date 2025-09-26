namespace Raven.CodeAnalysis;

internal partial class BoundTypeOfExpression : BoundExpression
{
    public BoundTypeOfExpression(ITypeSymbol operandType, ITypeSymbol systemType)
        : base(systemType)
    {
        OperandType = operandType;
        SystemType = systemType;
    }

    public ITypeSymbol OperandType { get; }

    public ITypeSymbol SystemType { get; }
}
