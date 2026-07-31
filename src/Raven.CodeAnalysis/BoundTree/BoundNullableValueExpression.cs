namespace Raven.CodeAnalysis;

internal partial class BoundNullableValueExpression : BoundExpression
{
    public BoundExpression Operand { get; }

    public ITypeSymbol ValueType { get; }
    public bool IsNullabilityFlowNarrowing { get; }

    public BoundNullableValueExpression(
        BoundExpression operand,
        ITypeSymbol valueType,
        bool isNullabilityFlowNarrowing = false)
        : base(valueType, symbol: null, BoundExpressionReason.None)
    {
        Operand = operand;
        ValueType = valueType;
        IsNullabilityFlowNarrowing = isNullabilityFlowNarrowing;
    }

    public override string ToString() => $"{Operand}.Value";
}
