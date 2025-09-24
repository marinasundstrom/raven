using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal partial class BoundBlockStatement : BoundStatement
{
    public IEnumerable<BoundStatement> Statements { get; }
    public ImmutableArray<ILocalSymbol> LocalsToDispose { get; }

    public BoundBlockStatement(IEnumerable<BoundStatement> statements, ImmutableArray<ILocalSymbol> localsToDispose = default)
    {
        Statements = statements;
        LocalsToDispose = localsToDispose.IsDefault ? ImmutableArray<ILocalSymbol>.Empty : localsToDispose;
    }
}
