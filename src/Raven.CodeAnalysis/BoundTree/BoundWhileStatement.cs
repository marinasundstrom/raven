namespace Raven.CodeAnalysis;

internal partial class BoundWhileStatement : BoundStatement
{
    public BoundExpression Condition { get; }
    public BoundStatement Body { get; }

    public BoundWhileStatement(BoundExpression condition, BoundStatement body)
    {
        Condition = condition;
        Body = body;
    }
}
