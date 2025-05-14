namespace Raven.CodeAnalysis;

class BoundAssignmentExpression : BoundExpression
{
    public ILocalSymbol Variable { get; }
    public BoundExpression Expression { get; }

    public BoundAssignmentExpression(ILocalSymbol variable, BoundExpression expression)
        : base(expression.Type, variable, CandidateReason.None)
    {
        Variable = variable;
        Expression = expression;
    }
}