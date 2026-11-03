namespace Raven.CodeAnalysis.Symbols;

// Metadata-source-independent association used by import and name lookup.
internal interface UnionCompanionSymbol
{
    bool TryGetAssociatedUnion(out UnionSymbol union);
}
