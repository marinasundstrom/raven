using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private BoundExpression StorageReceiver(BoundExpression receiver, TupleFieldSymbol field)
    {
        var result = (BoundExpression)Visit(receiver)!;
        foreach (var rest in field.StoragePath)
            result = new BoundMemberAccessExpression(result, rest);
        return result;
    }

    public override BoundNode? VisitMemberAccessExpression(BoundMemberAccessExpression node)
        => node.Receiver is { } receiver && node.Member is TupleFieldSymbol { StoragePath.IsDefaultOrEmpty: false } field
            ? new BoundMemberAccessExpression(StorageReceiver(receiver, field), field.UnderlyingField, node.Reason)
            : base.VisitMemberAccessExpression(node);

    public override BoundNode? VisitFieldAccess(BoundFieldAccess node)
        => node.Receiver is { } receiver && node.Field is TupleFieldSymbol { StoragePath.IsDefaultOrEmpty: false } field
            ? new BoundFieldAccess(StorageReceiver(receiver, field), field.UnderlyingField, node.Reason)
            : base.VisitFieldAccess(node);

    public override BoundNode? VisitFieldAssignmentExpression(BoundFieldAssignmentExpression node)
        => node.Receiver is { } receiver && node.Field is TupleFieldSymbol { StoragePath.IsDefaultOrEmpty: false } field
            ? new BoundFieldAssignmentExpression(StorageReceiver(receiver, field), field.UnderlyingField, (BoundExpression)Visit(node.Right)!, node.UnitType, requiresReceiverAddress: true)
            : base.VisitFieldAssignmentExpression(node);
}
