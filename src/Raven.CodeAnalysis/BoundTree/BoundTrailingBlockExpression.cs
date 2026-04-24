using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundTrailingBlockExpression : BoundExpression
{
    public BoundTrailingBlockExpression(
        BoundExpression receiver,
        ImmutableArray<BoundTrailingBlockEntry> entries,
        ITypeSymbol type,
        BoundExpressionReason reason = BoundExpressionReason.None)
        : base(type, null, reason)
    {
        Receiver = receiver;
        Entries = entries;
    }

    public BoundExpression Receiver { get; }
    public ImmutableArray<BoundTrailingBlockEntry> Entries { get; }
}

internal abstract partial class BoundTrailingBlockEntry : BoundNode
{
}

internal sealed partial class BoundTrailingBlockAssignmentEntry : BoundTrailingBlockEntry
{
    public BoundTrailingBlockAssignmentEntry(string name, SyntaxKind operatorTokenKind, BoundExpression value)
    {
        Name = name;
        OperatorTokenKind = operatorTokenKind;
        Value = value;
    }

    public string Name { get; }
    public SyntaxKind OperatorTokenKind { get; }
    public BoundExpression Value { get; }
}

internal sealed partial class BoundTrailingBlockExpressionEntry : BoundTrailingBlockEntry
{
    public BoundTrailingBlockExpressionEntry(BoundExpression expression)
    {
        Expression = expression;
    }

    public BoundExpression Expression { get; }
}
