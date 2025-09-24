using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

class Scope : Generator
{
    private readonly IDictionary<ISymbol, LocalBuilder> _localBuilders = new Dictionary<ISymbol, LocalBuilder>(SymbolEqualityComparer.Default);
    private readonly ImmutableArray<ILocalSymbol> _localsToDispose;

    public Scope(Generator parent) : this(parent, ImmutableArray<ILocalSymbol>.Empty)
    {
    }

    public Scope(Generator parent, ImmutableArray<ILocalSymbol> localsToDispose) : base(parent)
    {
        _localsToDispose = localsToDispose.IsDefault ? ImmutableArray<ILocalSymbol>.Empty : localsToDispose;
    }

    public override void AddLocal(ILocalSymbol localSymbol, LocalBuilder builder)
    {
        if (!_localBuilders.ContainsKey(localSymbol))
            _localBuilders.Add(localSymbol, builder);
    }

    public override LocalBuilder? GetLocal(ILocalSymbol localSymbol)
    {
        if (_localBuilders.TryGetValue(localSymbol, out var localBuilder))
            return localBuilder;

        return Parent?.GetLocal(localSymbol);
    }

    public override IEnumerable<ILocalSymbol> EnumerateLocalsToDispose()
    {
        for (int i = _localsToDispose.Length - 1; i >= 0; i--)
            yield return _localsToDispose[i];

        if (Parent is not null)
        {
            foreach (var local in Parent.EnumerateLocalsToDispose())
                yield return local;
        }
    }

    public ImmutableArray<ILocalSymbol> LocalsToDispose => _localsToDispose;
}
