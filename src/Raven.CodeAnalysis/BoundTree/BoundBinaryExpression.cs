namespace Raven.CodeAnalysis;

internal partial class BoundBinaryExpression : BoundExpression
{
    public BoundExpression Left { get; }
    public BoundBinaryOperator Operator { get; }
    public BoundExpression Right { get; }

    public BoundBinaryExpression(BoundExpression left, BoundBinaryOperator @operator, BoundExpression right)
        : base(@operator.ResultType, null, BoundExpressionReason.None)
    {
        Left = left;
        Operator = @operator;
        Right = right;
    }
}