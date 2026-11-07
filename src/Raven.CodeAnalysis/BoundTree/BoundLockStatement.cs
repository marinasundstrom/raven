namespace Raven.CodeAnalysis;

internal sealed partial class BoundLockStatement : BoundStatement
{
    public BoundLockStatement(BoundExpression expression, BoundStatement body)
    {
        Expression = expression;
        Body = body;
    }

    public BoundExpression Expression { get; }

    public BoundStatement Body { get; }
}
