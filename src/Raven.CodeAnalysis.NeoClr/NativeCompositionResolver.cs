using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// Composition imports need the exact native snapshots already registered by the host.
internal sealed class NativeCompositionResolver(IEnumerable<NeoClrMetadataDependency> dependencies) : IAssemblyResolver
{
    public AssemblyDefinition Resolve(AssemblyIdentity identity) => dependencies
        .Select(d => d.Reference is NeoClrMetadataReference native ? native.Definition : d.LegacyDefinition)
        .SingleOrDefault(d => d?.Identity.Equals(identity) == true)
        ?? throw new InvalidDataException("Unregistered composition dependency: " + identity.Name);
}
