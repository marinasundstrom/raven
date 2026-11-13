using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation : SourceContainerServices
{
    bool SourceContainerServices.NamespacesRepresentModules => Options.TargetPlatform == TargetPlatform.NeoCLR;

    bool BoundTreeOptimizationProvider.IsReleaseOptimizationEnabled => Options.OptimizationLevel == OptimizationLevel.Release;

    OperatorTypeProvider BoundTreeOptimizationProvider.OperatorTypes => this;

    void SourceContainerServices.EnsureSourceDeclarations(bool respectNamespaceLookupSuppression)
    {
        if (respectNamespaceLookupSuppression && (SourceDeclarationsDeclared || IsSourceNamespaceLookupDeclarationCompletionSuppressed))
            return;
        EnsureSourceDeclarationsDeclared();
    }

    NamedTypeSymbol? SourceContainerServices.FindReferencedType(AssemblySymbol assembly, string metadataName)
        => assembly is PEAssemblySymbol metadataAssembly ? metadataAssembly.GetTypeByMetadataName(metadataName) : null;
}
