namespace Raven.CodeAnalysis;

internal partial class BoundIfStatement : BoundStatement
{
    public BoundExpression Condition { get; }
    public BoundNode ThenNode { get; }
    public BoundNode? ElseNode { get; }

    public BoundIfStatement(BoundExpression condition, BoundNode thenNode, BoundNode? elseNode = null)
    {
        Condition = condition;
        ThenNode = thenNode;
        ElseNode = elseNode;
    }
}
