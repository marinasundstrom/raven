namespace Raven.CodeAnalysis;

internal partial class BoundRequiredResultExpression : BoundExpression
{
    public BoundExpression Operand { get; }

    public BoundRequiredResultExpression(BoundExpression operand)
        : base(operand.Type, symbol: null, BoundExpressionReason.None)
    {
        Operand = operand;
    }

    public override string ToString() => $"{Operand}";
}
