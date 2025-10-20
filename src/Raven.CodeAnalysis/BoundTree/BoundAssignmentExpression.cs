namespace Raven.CodeAnalysis;

abstract class BoundAssignmentExpression : BoundExpression
{
    public BoundExpression Right { get; }
    public ITypeSymbol UnitType { get; }

    protected BoundAssignmentExpression(ITypeSymbol unitType, BoundExpression right)
        : base(unitType, null, BoundExpressionReason.None)
    {
        Right = right;
        UnitType = unitType;
    }
}

internal partial class BoundLocalAssignmentExpression : BoundAssignmentExpression
{
    public ILocalSymbol Local { get; }

    public BoundLocalAssignmentExpression(ILocalSymbol local, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, right)
    {
        Local = local;
    }
}

internal partial class BoundPropertyAssignmentExpression : BoundAssignmentExpression
{
    public BoundExpression? Receiver { get; }
    public IPropertySymbol Property { get; }

    public BoundPropertyAssignmentExpression(BoundExpression? receiver, IPropertySymbol property, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, right)
    {
        Receiver = receiver;
        Property = property;
    }
}

internal partial class BoundFieldAssignmentExpression : BoundAssignmentExpression
{
    public BoundExpression? Receiver { get; }
    public IFieldSymbol Field { get; }
    public bool RequiresReceiverAddress { get; }

    public BoundFieldAssignmentExpression(BoundExpression? receiver, IFieldSymbol field, BoundExpression right, ITypeSymbol unitType, bool requiresReceiverAddress = false)
        : base(unitType, right)
    {
        Receiver = receiver;
        Field = field;
        RequiresReceiverAddress = requiresReceiverAddress;
    }
}

internal partial class BoundArrayAssignmentExpression : BoundAssignmentExpression
{
    public BoundArrayAccessExpression Left { get; }

    public BoundArrayAssignmentExpression(BoundArrayAccessExpression left, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, right)
    {
        Left = left;
    }
}

internal partial class BoundParameterAssignmentExpression : BoundAssignmentExpression
{
    public IParameterSymbol Parameter { get; }

    public BoundParameterAssignmentExpression(IParameterSymbol parameter, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, right)
    {
        Parameter = parameter;
    }
}

internal partial class BoundIndexerAssignmentExpression : BoundAssignmentExpression
{
    public BoundIndexerAccessExpression Left { get; }

    public BoundIndexerAssignmentExpression(BoundIndexerAccessExpression left, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, right)
    {
        Left = left;
    }
}

internal partial class BoundMemberAssignmentExpression : BoundAssignmentExpression
{
    public ISymbol Member { get; }
    public BoundExpression Receiver { get; }

    public BoundMemberAssignmentExpression(ISymbol member, BoundExpression receiver, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, right)
    {
        Member = member;
        Receiver = receiver;
    }
}

internal sealed partial class BoundPatternAssignmentExpression : BoundAssignmentExpression
{
    public BoundPattern Pattern { get; }

    public BoundPatternAssignmentExpression(ITypeSymbol type, BoundPattern pattern, BoundExpression right, ITypeSymbol unitType)
        : base(unitType, right)
    {
        Pattern = pattern;
    }
}
