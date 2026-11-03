namespace Raven.CodeAnalysis;

abstract class BoundAssignmentExpression : BoundExpression
{
    public BoundExpression Left { get; }
    public BoundExpression Right { get; }
    public TypeSymbol UnitType { get; }

    protected BoundAssignmentExpression(TypeSymbol unitType, BoundExpression left, BoundExpression right)
        : base(unitType, null, BoundExpressionReason.None)
    {
        Left = left;
        Right = right;
        UnitType = unitType;
    }
}

internal partial class BoundLocalAssignmentExpression : BoundAssignmentExpression
{
    public LocalSymbol Local { get; }

    public BoundLocalAccess LocalAccess { get; }

    public BoundLocalAssignmentExpression(LocalSymbol local, BoundExpression left, BoundExpression right, TypeSymbol unitType)
        : base(unitType, left, right)
    {
        Local = local;
        LocalAccess = (BoundLocalAccess)left;
    }
}

internal partial class BoundPropertyAssignmentExpression : BoundAssignmentExpression
{
    public BoundExpression? Receiver { get; }
    public PropertySymbol Property { get; }

    public BoundPropertyAssignmentExpression(BoundExpression? receiver, PropertySymbol property, BoundExpression left, BoundExpression right, TypeSymbol unitType)
        : base(unitType, left, right)
    {
        Receiver = receiver;
        Property = property;
    }
}

internal partial class BoundFieldAssignmentExpression : BoundAssignmentExpression
{
    public BoundExpression? Receiver { get; }
    public FieldSymbol Field { get; }
    public bool RequiresReceiverAddress { get; }

    public BoundFieldAssignmentExpression(BoundExpression? receiver, FieldSymbol field, BoundExpression right, TypeSymbol unitType, bool requiresReceiverAddress = false)
        : base(unitType, receiver, right)
    {
        Receiver = receiver;
        Field = field;
        RequiresReceiverAddress = requiresReceiverAddress;
    }
}

internal partial class BoundArrayAssignmentExpression : BoundAssignmentExpression
{
    public new BoundArrayAccessExpression Left { get; }

    public BoundArrayAssignmentExpression(BoundArrayAccessExpression left, BoundExpression right, TypeSymbol unitType)
        : base(unitType, left, right)
    {
        Left = left;
    }
}

internal partial class BoundParameterAssignmentExpression : BoundAssignmentExpression
{
    public ParameterSymbol Parameter { get; }

    public BoundParameterAssignmentExpression(ParameterSymbol parameter, BoundExpression left, BoundExpression right, TypeSymbol unitType)
    : base(unitType, left, right)
    {
        Parameter = parameter;
    }
}

internal partial class BoundIndexerAssignmentExpression : BoundAssignmentExpression
{
    public new BoundIndexerAccessExpression Left { get; }

    public BoundIndexerAssignmentExpression(BoundIndexerAccessExpression left, BoundExpression right, TypeSymbol unitType)
        : base(unitType, left, right)
    {
        Left = left;
    }
}

internal partial class BoundMemberAssignmentExpression : BoundAssignmentExpression
{
    public Symbol Member { get; }
    public BoundExpression Receiver { get; }

    public BoundMemberAssignmentExpression(Symbol member, BoundExpression receiver, BoundExpression right, TypeSymbol unitType)
        : base(unitType, receiver, right)
    {
        Member = member;
        Receiver = receiver;
    }
}

internal sealed partial class BoundPatternAssignmentExpression : BoundAssignmentExpression
{
    public BoundPattern Pattern { get; }

    public BoundPatternAssignmentExpression(TypeSymbol type, BoundPattern pattern, BoundExpression right, TypeSymbol unitType)
        : base(unitType, pattern, right)
    {
        Pattern = pattern;
    }
}
