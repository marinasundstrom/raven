namespace Raven.CodeAnalysis;

sealed partial class BoundAssignmentStatement : BoundStatement
{
    public BoundAssignmentExpression Expression { get; }

    public BoundAssignmentStatement(BoundAssignmentExpression expression)
    {
        Expression = expression;
    }
}

