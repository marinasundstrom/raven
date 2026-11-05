using System.Reflection;

using Mono.Cecil;

namespace Raven.CodeAnalysis.CodeGen;

// Cecil needs enum definitions when writing optional-parameter constants. Resolve
// those definitions from explicit compilation references, including image-backed
// references whose temporary file name differs from their assembly identity.
internal sealed class CompilationReferenceAssemblyResolver : DefaultAssemblyResolver
{
    private readonly Dictionary<string, string> _paths = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, AssemblyDefinition> _resolved = new(StringComparer.OrdinalIgnoreCase);

    public CompilationReferenceAssemblyResolver(IEnumerable<PortableExecutableReference> references)
    {
        foreach (var reference in references)
        {
            var identity = AssemblyName.GetAssemblyName(reference.FilePath);
            _paths.TryAdd(identity.FullName, reference.FilePath);
        }
    }

    public override AssemblyDefinition Resolve(AssemblyNameReference name, ReaderParameters parameters)
    {
        if (_resolved.TryGetValue(name.FullName, out var resolved))
            return resolved;
        if (!_paths.TryGetValue(name.FullName, out var path))
            return base.Resolve(name, parameters);

        var assembly = AssemblyDefinition.ReadAssembly(path, new ReaderParameters
        {
            AssemblyResolver = this,
            InMemory = true,
            ReadingMode = ReadingMode.Deferred
        });
        RegisterAssembly(assembly);
        _resolved.Add(name.FullName, assembly);
        return assembly;
    }
}
