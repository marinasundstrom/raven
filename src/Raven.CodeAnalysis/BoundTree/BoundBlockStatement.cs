using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class BoundBlockStatement : BoundStatement
{
    public IEnumerable<BoundStatement> Statements { get; }
    public ImmutableArray<LocalSymbol> LocalsToDispose { get; }
    public bool IntroduceILScope { get; }

    public BoundBlockStatement(
        IEnumerable<BoundStatement>? statements,
        ImmutableArray<LocalSymbol> localsToDispose = default,
        bool introduceILScope = true)
    {
        Statements = statements switch
        {
            null => ImmutableArray<BoundStatement>.Empty,
            ImmutableArray<BoundStatement> immutable => immutable,
            BoundStatement[] array => ImmutableArray.CreateRange(array),
            IEnumerable<BoundStatement> remaining => remaining.ToImmutableArray()
        };
        LocalsToDispose = localsToDispose.IsDefault ? ImmutableArray<LocalSymbol>.Empty : localsToDispose;
        IntroduceILScope = introduceILScope;
    }
}
