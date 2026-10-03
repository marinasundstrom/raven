namespace Raven.CodeAnalysis.Symbols;

// Metadata-source-independent association used by import and name lookup.
internal interface IUnionCompanionSymbol
{
    bool TryGetAssociatedUnion(out IUnionSymbol union);
}
