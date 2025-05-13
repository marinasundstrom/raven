namespace Raven.CodeAnalysis;

class BoundVariableExpression : BoundExpression
{
    public ILocalSymbol Variable { get; }
    public override ITypeSymbol Type => Variable.Type;

    public BoundVariableExpression(ILocalSymbol variable)
    {
        Variable = variable;
    }
}
