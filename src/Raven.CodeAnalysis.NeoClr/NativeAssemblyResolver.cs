using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// Resolves only explicit immutable inputs; no file probing or reflection loading.
internal sealed class NativeAssemblyResolver(IEnumerable<AssemblyDefinition> definitions) : IAssemblyResolver
{
    private readonly Dictionary<AssemblyIdentity, AssemblyDefinition> assemblies = definitions.ToDictionary(d => d.Identity);
    public AssemblyDefinition? Resolve(AssemblyIdentity identity) => assemblies.GetValueOrDefault(identity);
}
