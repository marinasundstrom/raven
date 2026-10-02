namespace Raven.CodeAnalysis.Metadata;

// Target adapters supply semantic references without routing them through reflection.
// This remains internal while the second provider establishes the contract.
internal interface ISemanticMetadataReference
{
    string? Validate(Compilation compilation);
    IAssemblySymbol CreateAssemblySymbol(Compilation compilation);
}

internal sealed class CompositeSemanticDataLoader(Compilation compilation, ISemanticDataLoader cli) : ISemanticDataLoader
{
    public IAssemblySymbol? LoadReference(MetadataReference reference)
        => reference is ISemanticMetadataReference semantic
            ? semantic.CreateAssemblySymbol(compilation)
            : cli.LoadReference(reference);
}
