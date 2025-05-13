namespace Raven.CodeAnalysis;

class BoundUnaryExpression : BoundExpression
{
    public BoundUnaryOperator Operator { get; }
    public BoundExpression Operand { get; }

    public override ITypeSymbol Type => Operator.ResultType;

    public BoundUnaryExpression(BoundUnaryOperator op, BoundExpression operand)
    {
        Operator = op;
        Operand = operand;
    }
}
