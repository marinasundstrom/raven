namespace Raven.CodeAnalysis;

abstract class BoundAssignmentExpression : BoundExpression
{
    public BoundExpression Right { get; }

    protected BoundAssignmentExpression(ITypeSymbol type, BoundExpression right)
        : base(type, null, BoundExpressionReason.None)
    {
        Right = right;
    }
}

internal partial class BoundLocalAssignmentExpression : BoundAssignmentExpression
{
    public ILocalSymbol Local { get; }

    public BoundLocalAssignmentExpression(ILocalSymbol local, BoundExpression right)
        : base(local.Type, right)
    {
        Local = local;
    }
}

internal partial class BoundPropertyAssignmentExpression : BoundAssignmentExpression
{
    public BoundExpression? Receiver { get; }
    public IPropertySymbol Property { get; }

    public BoundPropertyAssignmentExpression(BoundExpression? receiver, IPropertySymbol property, BoundExpression right)
        : base(property.Type, right)
    {
        Receiver = receiver;
        Property = property;
    }
}

internal partial class BoundFieldAssignmentExpression : BoundAssignmentExpression
{
    public BoundExpression? Receiver { get; }
    public IFieldSymbol Field { get; }

    public BoundFieldAssignmentExpression(BoundExpression? receiver, IFieldSymbol field, BoundExpression right)
        : base(field.Type, right)
    {
        Receiver = receiver;
        Field = field;
    }
}

internal partial class BoundArrayAssignmentExpression : BoundAssignmentExpression
{
    public BoundArrayAccessExpression Left { get; }

    public BoundArrayAssignmentExpression(BoundArrayAccessExpression left, BoundExpression right)
        : base(left.Type, right)
    {
        Left = left;
    }
}

internal partial class BoundIndexerAssignmentExpression : BoundAssignmentExpression
{
    public BoundIndexerAccessExpression Left { get; }

    public BoundIndexerAssignmentExpression(BoundIndexerAccessExpression left, BoundExpression right)
        : base(left.Type, right)
    {
        Left = left;
    }
}

internal partial class BoundMemberAssignmentExpression : BoundAssignmentExpression
{
    public ISymbol Member { get; }
    public BoundExpression Receiver { get; }

    public BoundMemberAssignmentExpression(ISymbol member, BoundExpression receiver, BoundExpression right)
        : base(((IPropertySymbol)member).Type, right) // or field type
    {
        Member = member;
        Receiver = receiver;
    }
}