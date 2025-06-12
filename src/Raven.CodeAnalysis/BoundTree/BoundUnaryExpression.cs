namespace Raven.CodeAnalysis;

internal partial class BoundUnaryExpression : BoundExpression
{
    public BoundUnaryOperator Operator { get; }
    public BoundExpression Operand { get; }

    public BoundUnaryExpression(BoundUnaryOperator @operator, BoundExpression operand)
        : base(@operator.ResultType, null, BoundExpressionReason.None)
    {
        Operator = @operator;
        Operand = operand;
    }
}