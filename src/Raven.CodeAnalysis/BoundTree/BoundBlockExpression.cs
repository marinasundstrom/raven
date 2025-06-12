namespace Raven.CodeAnalysis;

internal partial class BoundBlockExpression : BoundExpression
{
    public BoundBlockExpression(IEnumerable<BoundStatement> statements)
        : base(statements.LastOrDefault()?.Type, null, BoundExpressionReason.None)
    {
        Statements = statements;
    }

    public IEnumerable<BoundStatement> Statements { get; }
}