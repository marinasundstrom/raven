using System;
using System.Collections.Concurrent;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

/// <summary>
/// Central cache for symbols keyed by <see cref="DeclKey"/>.
/// </summary>
internal sealed class SymbolFactory
{
    private readonly ConcurrentDictionary<DeclKey, Lazy<Symbol>> _cache = new();

    public TSymbol GetOrCreate<TSymbol>(DeclKey key, Func<TSymbol> build)
        where TSymbol : Symbol
        => (TSymbol)_cache.GetOrAdd(key, _ => new Lazy<Symbol>(() => build(), true)).Value;
}
