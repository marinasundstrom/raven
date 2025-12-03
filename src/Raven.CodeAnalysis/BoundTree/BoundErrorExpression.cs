using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal partial class BoundErrorExpression : BoundExpression
{
    public BoundErrorExpression(
        ITypeSymbol type,
        ISymbol? symbol = null,
        BoundExpressionReason reason = BoundExpressionReason.None,
        ImmutableArray<ISymbol> candidates = default)
        : base(type, symbol, reason)
    {
        Candidates = candidates.IsDefault ? ImmutableArray<ISymbol>.Empty : candidates;
    }

    public ImmutableArray<ISymbol> Candidates { get; }
}