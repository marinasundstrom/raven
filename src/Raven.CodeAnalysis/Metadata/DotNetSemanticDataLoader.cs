using System;
using System.Collections.Concurrent;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Metadata;

// Per-compilation symbol ownership; only the underlying metadata session may
// be reused by another snapshot. Reflection and PE symbol construction stay here.
internal sealed class DotNetSemanticDataLoader(Compilation compilation, DotNetMetadataSession session) : ISemanticDataLoader
{
    private readonly Compilation _compilation = compilation;
    private readonly DotNetMetadataSession _session = session;
    private readonly ConcurrentDictionary<Assembly, IAssemblySymbol> _assemblySymbols = new();
    private readonly ConcurrentDictionary<string, Assembly> _lazyMetadataAssemblies = new();

    public IAssemblySymbol? LoadReference(MetadataReference reference)
    {
        if (reference is not PortableExecutableReference portable)
            throw new InvalidOperationException();

        Assembly assembly;
        try
        {
            assembly = LoadMetadataAssembly(portable.FilePath);
        }
        catch (BadImageFormatException)
        {
            // Mixed MSBuild inputs may include native PE files. Preserve their
            // existing omission while continuing to load managed references.
            return null;
        }

        _compilation.RegisterRuntimeAssembly(assembly, portable.FilePath);
        return GetAssembly(assembly, portable.FilePath);
    }

    private Assembly LoadMetadataAssembly(string assemblyPath)
    {
        var fullPath = Path.GetFullPath(assemblyPath);
        if (_lazyMetadataAssemblies.TryGetValue(fullPath, out var cachedByPath))
            return cachedByPath;

        System.Reflection.AssemblyName? identity = null;
        try
        {
            identity = Compilation.ReadAssemblyName(fullPath);
            if (identity.Name is not null)
            {
                _compilation.RegisterMetadataAssemblyPath(identity.Name, fullPath);
            }
        }
        catch
        {
            // Fall through and attempt to load by path directly.
        }

        var assembly = _session.LoadFromPath(fullPath, identity);

        _lazyMetadataAssemblies[fullPath] = assembly;

        return assembly;
    }

    private IAssemblySymbol GetAssembly(Assembly assembly, string? assemblyPathOverride = null)
    {
        _compilation.RegisterRuntimeAssembly(assembly);

        if (_assemblySymbols.TryGetValue(assembly, out var asss))
        {
            if (asss is PEAssemblySymbol peAssembly)
                peAssembly.SetAssemblyPath(assemblyPathOverride);

            return asss;
        }

        string? assemblyPath = assemblyPathOverride;
        var identity = assembly.GetName();
        if (assemblyPath is null && identity.Name is not null)
            assemblyPath = _compilation.GetRegisteredMetadataAssemblyPath(identity.Name);
        PEAssemblySymbol assemblySymbol = new PEAssemblySymbol(assembly, [], assemblyPath);
        _assemblySymbols[assembly] = assemblySymbol;

        var refs = assembly.GetReferencedAssemblies();

        assemblySymbol.AddModules(
            new PEModuleSymbol(
                _compilation.ReflectionTypeLoader,
                assemblySymbol,
                assembly.ManifestModule,
                [],
                refs.Select(x =>
                {
                    try
                    {
                        var loadedAssembly = _session.LoadFromAssemblyName(x);
                        if (loadedAssembly is null)
                            return null;

                        _compilation.RegisterRuntimeAssembly(loadedAssembly);
                        return GetAssembly(loadedAssembly);
                    }
                    catch
                    {
                        return null;
                    }
                }).OfType<IAssemblySymbol>()));

        return assemblySymbol;
    }
}
