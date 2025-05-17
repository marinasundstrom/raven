using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class MetadataSymbol : Symbol
{
    protected readonly Compilation _compilation;

    protected MetadataSymbol(Compilation compilation, ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations) : base(containingSymbol, containingType, containingNamespace, locations, [])
    {
        _compilation = compilation;
    }

    public override Compilation Compilation => _compilation;
}