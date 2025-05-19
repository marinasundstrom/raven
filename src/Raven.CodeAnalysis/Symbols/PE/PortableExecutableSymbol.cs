using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class PortableExecutableSymbol : Symbol
{
    protected PortableExecutableSymbol(ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations) : base(containingSymbol, containingType, containingNamespace, locations, [])
    {

    }

    public override Compilation Compilation => ContainingNamespace!.Compilation;
}