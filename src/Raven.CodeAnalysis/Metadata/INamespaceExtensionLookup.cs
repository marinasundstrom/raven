using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Metadata;

// A discovery capability, not a namespace origin marker. Composite namespaces
// can aggregate providers without becoming metadata implementations themselves.
internal interface INamespaceExtensionLookup
{
    // Returns indexed candidate containers for this namespace and method name.
    // Source declaration traversal and receiver applicability are owned by binding.
    ImmutableArray<INamedTypeSymbol> GetExtensionMethodContainers(string methodName);
}
