namespace Raven.CodeAnalysis;

class BoundAssignmentExpression : BoundExpression
{
    public ILocalSymbol Variable { get; }
    public BoundExpression Expression { get; }

    public override ITypeSymbol Type => Expression.Type;

    public BoundAssignmentExpression(ILocalSymbol variable, BoundExpression expression)
    {
        Variable = variable;
        Expression = expression;
    }
}