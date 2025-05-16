namespace Raven.CodeAnalysis;

class BoundBlockExpression : BoundExpression
{
    public BoundBlockExpression(IReadOnlyList<BoundExpression> statements)
        : base(statements.LastOrDefault()?.Type, null, BoundExpressionReason.None)
    {
        Statements = statements;
    }

    public IReadOnlyList<BoundExpression> Statements { get; }
}