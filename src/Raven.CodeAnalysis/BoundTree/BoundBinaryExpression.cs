namespace Raven.CodeAnalysis;

internal partial class BoundBinaryExpression : BoundExpression
{
    public BoundExpression Left { get; }
    public BoundBinaryOperator Operator { get; }
    public BoundExpression Right { get; }

    public BoundBinaryExpression(BoundExpression left, BoundBinaryOperator op, BoundExpression right)
        : base(op.ResultType, null, BoundExpressionReason.None)
    {
        Left = left;
        Operator = op;
        Right = right;
    }
}