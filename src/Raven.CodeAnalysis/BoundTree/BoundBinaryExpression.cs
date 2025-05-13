namespace Raven.CodeAnalysis;

class BoundBinaryExpression : BoundExpression
{
    public BoundExpression Left { get; }
    public BoundBinaryOperator Operator { get; }
    public BoundExpression Right { get; }

    public override ITypeSymbol Type => Operator.ResultType;

    public BoundBinaryExpression(BoundExpression left, BoundBinaryOperator op, BoundExpression right)
    {
        Left = left;
        Operator = op;
        Right = right;
    }
}
