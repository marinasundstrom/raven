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
        IMethodSymbol? method,
        ImmutableArray<IMethodSymbol> memberMethods,
        ImmutableArray<ISymbol> parameterMembers,
        IMethodSymbol? cloneMethod,
        IMethodSymbol? copyConstructor,
        ITypeSymbol type,
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
    public IMethodSymbol? Method { get; }
    public ImmutableArray<IMethodSymbol> MemberMethods { get; }
    public ImmutableArray<ISymbol> ParameterMembers { get; }
    public IMethodSymbol? CloneMethod { get; }
    public IMethodSymbol? CopyConstructor { get; }
}

internal sealed partial class BoundWithAssignment : BoundNode
{
    public BoundWithAssignment(ISymbol member, BoundExpression value)
    {
        Member = member;
        Value = value;
    }

    public ISymbol Member { get; }
    public BoundExpression Value { get; }
}
