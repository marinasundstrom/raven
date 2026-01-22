
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
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
/// A property/field assignment entry: <c>Name = Expression</c>
/// </summary>
internal sealed partial class BoundObjectInitializerAssignmentEntry : BoundObjectInitializerEntry
{
    public ISymbol Member { get; }
    public BoundExpression Value { get; }

    public BoundObjectInitializerAssignmentEntry(ISymbol member, BoundExpression value)
    {
        Member = member;
        Value = value;
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
