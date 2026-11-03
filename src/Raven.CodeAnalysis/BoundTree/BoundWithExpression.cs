using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal enum BoundWithStrategyKind
{
    RecordClone,
    UpdateMethod,
    WithMethod,
    WithMemberMethods,
    Clone
}

internal sealed partial class BoundWithExpression : BoundExpression
{
    public BoundWithExpression(
        BoundExpression receiver,
        ImmutableArray<BoundWithAssignment> assignments,
        BoundWithStrategyKind strategy,
        MethodSymbol? method,
        ImmutableArray<MethodSymbol> memberMethods,
        ImmutableArray<Symbol> parameterMembers,
        MethodSymbol? cloneMethod,
        MethodSymbol? copyConstructor,
        TypeSymbol type,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, method ?? cloneMethod ?? copyConstructor, reason)
    {
        Receiver = receiver;
        Assignments = assignments;
        Strategy = strategy;
        Method = method;
        MemberMethods = memberMethods;
        ParameterMembers = parameterMembers;
        CloneMethod = cloneMethod;
        CopyConstructor = copyConstructor;
    }

    public BoundExpression Receiver { get; }
    public ImmutableArray<BoundWithAssignment> Assignments { get; }
    public BoundWithStrategyKind Strategy { get; }
    public MethodSymbol? Method { get; }
    public ImmutableArray<MethodSymbol> MemberMethods { get; }
    public ImmutableArray<Symbol> ParameterMembers { get; }
    public MethodSymbol? CloneMethod { get; }
    public MethodSymbol? CopyConstructor { get; }
}

internal sealed partial class BoundWithAssignment : BoundNode
{
    public BoundWithAssignment(Symbol member, BoundExpression value)
    {
        Member = member;
        Value = value;
    }

    public Symbol Member { get; }
    public BoundExpression Value { get; }
}
