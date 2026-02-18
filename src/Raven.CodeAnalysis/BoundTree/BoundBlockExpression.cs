using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundBlockExpression : BoundExpression
{
    public BoundBlockExpression(IEnumerable<BoundStatement> statements, ITypeSymbol unitType, ImmutableArray<ILocalSymbol> localsToDispose = default)
        : this(MaterializeStatements(statements), unitType, localsToDispose)
    {
    }

    private BoundBlockExpression(ImmutableArray<BoundStatement> statements, ITypeSymbol unitType, ImmutableArray<ILocalSymbol> localsToDispose)
        : base((statements.LastOrDefault() as BoundExpressionStatement)?.Expression.Type ?? unitType, null, BoundExpressionReason.None)
    {
        Statements = statements;
        UnitType = unitType;
        LocalsToDispose = localsToDispose.IsDefault ? ImmutableArray<ILocalSymbol>.Empty : localsToDispose;
    }

    public IEnumerable<BoundStatement> Statements { get; }
    public ITypeSymbol UnitType { get; }
    public ImmutableArray<ILocalSymbol> LocalsToDispose { get; }

    private static ImmutableArray<BoundStatement> MaterializeStatements(IEnumerable<BoundStatement> statements)
    {
        return statements switch
        {
            null => ImmutableArray<BoundStatement>.Empty,
            ImmutableArray<BoundStatement> immutable => immutable,
            BoundStatement[] array => ImmutableArray.CreateRange(array),
            _ => statements.ToImmutableArray()
        };
    }
}
