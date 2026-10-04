using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Target-owned semantic shape for arrays whose base type supplies this capability.
/// Additional interfaces exclude those already inherited from the base type.
/// Members retain their declaring owners for normal dispatch and emission.
/// </summary>
internal interface IArrayTypeProvider
{
    // Source-owned target shapes remain provisional during declaration binding.
    bool AreInterfacesComplete { get; }

    ImmutableArray<INamedTypeSymbol> GetAdditionalInterfaces(IArrayTypeSymbol array);
    ImmutableArray<ISymbol> GetMembers(IArrayTypeSymbol array);
}
