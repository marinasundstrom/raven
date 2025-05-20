using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class PESymbol : Symbol
{
    protected PESymbol(ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations) : base(containingSymbol, containingType, containingNamespace, locations, [])
    {

    }

    public override IAssemblySymbol ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override IModuleSymbol ContainingModule => ContainingNamespace?.ContainingModule;

    protected PEAssemblySymbol PEContainingAssembly => (PEAssemblySymbol)ContainingAssembly;

    protected PEModuleSymbol PEContainingModule => (PEModuleSymbol)ContainingModule;
}