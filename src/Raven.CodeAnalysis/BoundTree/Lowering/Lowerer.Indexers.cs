using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitIndexerAccessExpression(BoundIndexerAccessExpression node)
    {
        if (!node.Captures.Any())
            return base.VisitIndexerAccessExpression(node);
        var access = new BoundIndexerAccessExpression(node.Receiver, node.Arguments, node.Indexer);
        return VisitExpression(new BoundBlockExpression(
            node.Captures.Append(new BoundExpressionStatement(access)), node.Type!));
    }

    public override BoundNode? VisitIndexerAssignmentExpression(BoundIndexerAssignmentExpression node)
    {
        if (!node.Left.Captures.Any())
            return base.VisitIndexerAssignmentExpression(node);
        var access = new BoundIndexerAccessExpression(node.Left.Receiver, node.Left.Arguments, node.Left.Indexer);
        var assignment = new BoundIndexerAssignmentExpression(access, node.Right, node.UnitType);
        return VisitExpression(new BoundBlockExpression(
            node.Left.Captures.Append(new BoundExpressionStatement(assignment)), node.Type!));
    }
}
