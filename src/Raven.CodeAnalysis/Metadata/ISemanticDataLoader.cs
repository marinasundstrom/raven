namespace Raven.CodeAnalysis.Metadata;

// The target's per-compilation importer. Results belong to this compilation's
// symbol universe; implementations must not reuse symbols from another snapshot.
// MetadataReference/IAssemblySymbol are the current reference surface, not a
// requirement that future targets read CLI metadata or execute imported code.
internal interface ISemanticDataLoader
{
    // Null preserves omission of inputs that do not contribute semantic data
    // (such as native files in an MSBuild reference list for the .NET target).
    IAssemblySymbol? LoadReference(MetadataReference reference);
}
