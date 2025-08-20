namespace Raven.CodeAnalysis;

using System.Collections.Immutable;

/// <summary>
/// Represents a syntax-only identity for a declaration. Used as a key for symbol caching.
/// </summary>
/// <param name="Kind">Kind of symbol (method, type, etc.).</param>
/// <param name="Name">Name of the declaration.</param>
/// <param name="Arity">Generic arity, if any.</param>
/// <param name="Containing">Key of the containing declaration.</param>
/// <param name="Locations">Locations of the declaration in source.</param>
internal sealed record class DeclKey(
    SymbolKind Kind,
    string Name,
    int Arity,
    DeclKey? Containing,
    ImmutableArray<Location> Locations);
