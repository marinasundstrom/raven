namespace Raven.CodeAnalysis;

internal partial class BoundVariableExpression : BoundExpression
{
    public LocalSymbol Variable { get; }

    public BoundVariableExpression(LocalSymbol variable)
        : base(variable.Type, variable, BoundExpressionReason.None)
    {
        Variable = variable;
    }
}
