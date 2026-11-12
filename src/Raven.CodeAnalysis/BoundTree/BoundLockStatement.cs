namespace Raven.CodeAnalysis;

internal sealed partial class BoundLockStatement(BoundExpression expression, BoundStatement body) : BoundStatement
{
    public BoundExpression Expression { get; } = expression;

    public BoundStatement Body { get; } = body;
}
