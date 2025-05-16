namespace Raven.CodeAnalysis;

class BoundVariableExpression : BoundExpression
{
    public ILocalSymbol Variable { get; }

    public BoundVariableExpression(ILocalSymbol variable)
        : base(variable.Type, variable, BoundExpressionReason.None)
    {
        Variable = variable;
    }
}