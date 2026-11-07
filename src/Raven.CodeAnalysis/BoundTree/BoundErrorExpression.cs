using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundErrorExpression : BoundExpression
{
    public BoundErrorExpression(
        TypeSymbol type,
        Symbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<Symbol> candidates = default)
        : base(type, symbol, reason)
    {
        Candidates = candidates.IsDefault ? ImmutableArray<Symbol>.Empty : candidates;
    }

    public ImmutableArray<Symbol> Candidates { get; }
}
