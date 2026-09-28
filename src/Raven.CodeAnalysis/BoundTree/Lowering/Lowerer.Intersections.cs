using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private Dictionary<ILocalSymbol, ILocalSymbol>? _intersectionLocals;

    private static bool IsReferenceIntersection(ITypeSymbol? type)
        => type is IIntersectionTypeSymbol intersection &&
            intersection.ConstituentTypes.All(static t => t is INamedTypeSymbol && t.IsReferenceType && !t.ContainsErrorType());

    public override BoundNode? VisitVariableDeclarator(BoundVariableDeclarator node)
    {
        if (!IsReferenceIntersection(node.Local.Type) || node.FixedAddressInitializer is not null)
            return base.VisitVariableDeclarator(node);

        // Keep the source symbol intact; only this lowering scope owns erased storage.
        var storage = CreateTempLocal(node.Local.Name, GetCompilation().GetSpecialType(SpecialType.System_Object), node.Local.IsMutable);
        (_intersectionLocals ??= new(SymbolEqualityComparer.Default)).Add(node.Local, storage);
        var initializer = (BoundExpression?)VisitExpression(node.Initializer);
        return new BoundVariableDeclarator(storage, initializer);
    }

    public override BoundNode? VisitLocalAccess(BoundLocalAccess node)
        => _intersectionLocals is not null && _intersectionLocals.TryGetValue(node.Local, out var storage)
            ? new BoundLocalAccess(storage)
            : base.VisitLocalAccess(node);

    public override BoundNode? VisitLocalAssignmentExpression(BoundLocalAssignmentExpression node)
    {
        if (_intersectionLocals is null || !_intersectionLocals.TryGetValue(node.Local, out var storage))
            return base.VisitLocalAssignmentExpression(node);

        return new BoundLocalAssignmentExpression(storage, new BoundLocalAccess(storage),
            (BoundExpression)VisitExpression(node.Right)!, node.UnitType);
    }

    public override BoundNode? VisitMemberAccessExpression(BoundMemberAccessExpression node)
    {
        var receiver = (BoundExpression?)VisitExpression(node.Receiver);
        receiver = ProjectIntersectionReceiver(node.Receiver, receiver, node.Member.ContainingType);
        return node.Update(receiver, node.Member, node.Reason);
    }

    public override BoundNode? VisitPropertyAssignmentExpression(BoundPropertyAssignmentExpression node)
    {
        var rewritten = (BoundPropertyAssignmentExpression)base.VisitPropertyAssignmentExpression(node)!;
        var receiver = ProjectIntersectionReceiver(node.Receiver, rewritten.Receiver, node.Property.ContainingType);
        return rewritten.Update(receiver, rewritten.Property, rewritten.Left, rewritten.Right, rewritten.UnitType);
    }

    public override BoundNode? VisitFieldAssignmentExpression(BoundFieldAssignmentExpression node)
    {
        var rewritten = (BoundFieldAssignmentExpression)base.VisitFieldAssignmentExpression(node)!;
        var receiver = ProjectIntersectionReceiver(node.Receiver, rewritten.Receiver, node.Field.ContainingType);
        return rewritten.Update(receiver, rewritten.Field, rewritten.Right, rewritten.UnitType, rewritten.RequiresReceiverAddress);
    }

    public override BoundNode? VisitIndexerAccessExpression(BoundIndexerAccessExpression node)
    {
        var rewritten = (BoundIndexerAccessExpression)base.VisitIndexerAccessExpression(node)!;
        var receiver = ProjectIntersectionReceiver(node.Receiver, rewritten.Receiver, node.Indexer.ContainingType)!;
        return rewritten.Update(receiver, rewritten.Arguments, rewritten.Indexer);
    }

    private BoundExpression? ProjectIntersectionReceiver(
        BoundExpression? original, BoundExpression? rewritten, ITypeSymbol? owner)
    {
        if (!IsReferenceIntersection(original?.Type) || rewritten is null || owner is null ||
            SymbolEqualityComparer.Default.Equals(original!.Type, rewritten.Type))
            return rewritten;

        return new BoundConversionExpression(rewritten, owner, new Conversion(isImplicit: false, isReference: true));
    }

    private BoundExpression? LowerIntersectionConversion(BoundConversionExpression node, BoundExpression rewritten)
    {
        if (!node.Conversion.IsIdentity && !node.Conversion.IsReference)
            return null;

        if (IsReferenceIntersection(node.Type) && node.Conversion.IsImplicit)
        {
            var objectType = GetCompilation().GetSpecialType(SpecialType.System_Object);
            return SymbolEqualityComparer.Default.Equals(rewritten.Type, objectType)
                ? rewritten
                : new BoundConversionExpression(rewritten, objectType, new Conversion(isImplicit: true, isReference: true));
        }

        if (IsReferenceIntersection(node.Expression.Type) &&
            !SymbolEqualityComparer.Default.Equals(rewritten.Type, node.Expression.Type))
        {
            // A semantic implicit projection needs a CLI cast after storage erasure.
            return new BoundConversionExpression(rewritten, node.Type!,
                new Conversion(isImplicit: false, isReference: true), node.IsNullableSuppression);
        }

        return null;
    }
}
