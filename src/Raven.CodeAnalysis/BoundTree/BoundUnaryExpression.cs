namespace Raven.CodeAnalysis;

class BoundUnaryExpression : BoundExpression
{
    public BoundUnaryOperator Operator { get; }
    public BoundExpression Operand { get; }

    public BoundUnaryExpression(BoundUnaryOperator op, BoundExpression operand)
        : base(op.ResultType, null, CandidateReason.None)
    {
        Operator = op;
        Operand = operand;
    }
}