namespace Raven.CodeAnalysis;

internal sealed partial class BoundIfStatement : BoundStatement
{
    public BoundExpression Condition { get; }
    public BoundStatement ThenNode { get; }
    public BoundStatement? ElseNode { get; }

    public BoundIfStatement(BoundExpression condition, BoundStatement thenNode, BoundStatement? elseNode = null)
    {
        Condition = condition;
        ThenNode = thenNode;
        ElseNode = elseNode;
    }
}
