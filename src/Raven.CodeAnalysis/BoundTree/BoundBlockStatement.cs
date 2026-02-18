using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundBlockStatement : BoundStatement
{
    public IEnumerable<BoundStatement> Statements { get; }
    public ImmutableArray<ILocalSymbol> LocalsToDispose { get; }

    public BoundBlockStatement(IEnumerable<BoundStatement> statements, ImmutableArray<ILocalSymbol> localsToDispose = default)
    {
        Statements = statements switch
        {
            null => ImmutableArray<BoundStatement>.Empty,
            ImmutableArray<BoundStatement> immutable => immutable,
            BoundStatement[] array => ImmutableArray.CreateRange(array),
            _ => statements.ToImmutableArray()
        };
        LocalsToDispose = localsToDispose.IsDefault ? ImmutableArray<ILocalSymbol>.Empty : localsToDispose;
    }
}
