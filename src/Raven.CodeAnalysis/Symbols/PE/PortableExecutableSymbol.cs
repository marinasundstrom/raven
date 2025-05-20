using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class PortableExecutableSymbol : Symbol
{
    protected PortableExecutableSymbol(ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations) : base(containingSymbol, containingType, containingNamespace, locations, [])
    {

    }

    public override IAssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override IModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule;

    protected PortableExecutableAssemblySymbol PEContainingAssembly => (PortableExecutableAssemblySymbol)ContainingAssembly;

    protected PortableExecutableModuleSymbol PEContainingModule => (PortableExecutableModuleSymbol)ContainingModule;
}