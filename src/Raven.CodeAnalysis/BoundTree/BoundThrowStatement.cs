namespace Raven.CodeAnalysis;

internal partial class BoundThrowStatement : BoundStatement
{
    public BoundThrowStatement(BoundExpression expression)
    {
        Expression = expression;
    }

    public BoundExpression Expression { get; }
}
