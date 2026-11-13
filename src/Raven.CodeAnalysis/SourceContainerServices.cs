namespace Raven.CodeAnalysis;

// Compilation-owned policy and declaration completion for source containers.
internal interface SourceContainerServices : SourceDeclarationServices, BoundTreeOptimizationProvider
{
    bool NamespacesRepresentModules { get; }
    void EnsureSourceDeclarations(bool respectNamespaceLookupSuppression);
    NamedTypeSymbol? FindReferencedType(AssemblySymbol assembly, string metadataName);
}
