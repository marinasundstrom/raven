namespace Raven.CodeAnalysis;

internal sealed class BoundYieldBreakStatement : BoundStatement
{
    public override void Accept(BoundTreeVisitor visitor) => visitor.VisitYieldBreakStatement(this);

    public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor) => visitor.VisitYieldBreakStatement(this);
}
