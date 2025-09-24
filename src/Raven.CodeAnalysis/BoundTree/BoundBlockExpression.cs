using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundBlockExpression : BoundExpression
{
    public BoundBlockExpression(IEnumerable<BoundStatement> statements, ITypeSymbol unitType, ImmutableArray<ILocalSymbol> localsToDispose = default)
        : base((statements.LastOrDefault() as BoundExpressionStatement)?.Expression.Type ?? unitType, null, BoundExpressionReason.None)
    {
        Statements = statements;
        UnitType = unitType;
        LocalsToDispose = localsToDispose.IsDefault ? ImmutableArray<ILocalSymbol>.Empty : localsToDispose;
    }

    public IEnumerable<BoundStatement> Statements { get; }
    public ITypeSymbol UnitType { get; }
    public ImmutableArray<ILocalSymbol> LocalsToDispose { get; }
}

