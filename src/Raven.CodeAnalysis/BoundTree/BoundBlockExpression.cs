namespace Raven.CodeAnalysis;

internal partial class BoundBlockExpression : BoundExpression
{
    public BoundBlockExpression(IReadOnlyList<BoundStatement> statements)
        : base(statements.LastOrDefault()?.Type, null, BoundExpressionReason.None)
    {
        Statements = statements;
    }

    public IReadOnlyList<BoundStatement> Statements { get; }
}