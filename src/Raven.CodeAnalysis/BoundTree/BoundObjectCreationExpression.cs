
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
namespace Raven.CodeAnalysis;

internal partial class BoundObjectCreationExpression : BoundExpression
{
    public IMethodSymbol Constructor { get; }
    public IEnumerable<BoundExpression> Arguments { get; }
    public BoundExpression? Receiver { get; }
    public BoundObjectInitializer? Initializer { get; }

    public BoundObjectCreationExpression(IMethodSymbol constructor, IEnumerable<BoundExpression> arguments, BoundExpression? receiver = null, BoundObjectInitializer? initializer = null)
           : base(constructor.ContainingType!, constructor, BoundExpressionReason.None)
    {
        Constructor = constructor;
        Arguments = arguments;
        Initializer = initializer;
        Receiver = receiver;
    }

    public override string ToString()
    {
        var args = string.Join(", ", Arguments.Select(a => a.ToString()));
        var init = Initializer is null ? string.Empty : " { ... }";
        return $"new {Constructor.ContainingType.Name}({args}){init}";
    }
}


internal sealed partial class BoundObjectInitializer : BoundNode
{
    public IEnumerable<BoundObjectInitializerEntry> Entries { get; }

    public BoundObjectInitializer(IEnumerable<BoundObjectInitializerEntry> entries)
    {
        Entries = entries is ImmutableArray<BoundObjectInitializerEntry> a
            ? a
            : ImmutableArray.CreateRange(entries);
    }
}

internal abstract partial class BoundObjectInitializerEntry : BoundNode
{
}

/// <summary>
/// A member assignment entry: <c>Name = Expression</c> or <c>Name op= Expression</c>.
/// </summary>
internal sealed partial class BoundObjectInitializerAssignmentEntry : BoundObjectInitializerEntry
{
    public ISymbol Member { get; }
    public SyntaxKind OperatorTokenKind { get; }
    public BoundExpression Value { get; }

    public BoundObjectInitializerAssignmentEntry(ISymbol member, SyntaxKind operatorTokenKind, BoundExpression value)
    {
        Member = member;
        OperatorTokenKind = operatorTokenKind;
        Value = value;
    }

    public BoundObjectInitializerAssignmentEntry(ISymbol member, BoundExpression value)
        : this(member, SyntaxKind.EqualsToken, value)
    {
    }
}

/// <summary>
/// A content entry: a standalone expression (typically a nested object construction),
/// to be forwarded to the parent's content target during lowering.
/// </summary>
internal sealed partial class BoundObjectInitializerExpressionEntry : BoundObjectInitializerEntry
{
    public BoundExpression Expression { get; }

    public BoundObjectInitializerExpressionEntry(BoundExpression expression)
    {
        Expression = expression;
    }
}
