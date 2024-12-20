using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class MetadataSymbol : Symbol
{
    protected MetadataSymbol(ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations) : base(containingSymbol, containingType, containingNamespace, locations, [])
    {

    }
}