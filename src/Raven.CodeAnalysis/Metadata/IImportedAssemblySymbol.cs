using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Metadata;

// Compiler-owned discovery operations for an imported assembly. Implementations
// return semantic symbols; metadata readers and indexing stay provider-private.
internal interface IImportedAssemblySymbol : IAssemblySymbol
{
    // Includes nested types, matched by their own name and arity. Returns the
    // provider's first matching type, or null when no match exists.
    INamedTypeSymbol? GetTypeBySimpleName(string name, int arity);

    // Candidate containers only: applicability remains a binding decision.
    ImmutableArray<INamedTypeSymbol> GetExtensionConversionContainers();
}
