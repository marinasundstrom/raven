namespace Raven.CodeAnalysis;

internal sealed class BoundYieldReturnStatement : BoundStatement
{
    public BoundYieldReturnStatement(BoundExpression expression)
    {
        Expression = expression;
    }

    public BoundExpression Expression { get; }

    public override void Accept(BoundTreeVisitor visitor) => visitor.VisitYieldReturnStatement(this);

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => visitor.VisitYieldReturnStatement(this);
}
