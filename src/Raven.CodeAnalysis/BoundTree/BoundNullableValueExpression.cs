namespace Raven.CodeAnalysis;

internal partial class BoundNullableValueExpression : BoundExpression
{
    public BoundExpression Operand { get; }

    public ITypeSymbol ValueType { get; }

    public BoundNullableValueExpression(BoundExpression operand, ITypeSymbol valueType)
        : base(valueType, symbol: null, BoundExpressionReason.None)
    {
        Operand = operand;
        ValueType = valueType;
    }

    public override string ToString() => $"{Operand}.Value";
}
