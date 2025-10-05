namespace System.Reflection2;

using System.Collections.Concurrent;
using System.Diagnostics.CodeAnalysis;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.IO;

/// <summary>
/// Resolves assemblies from a fixed set of search paths.
/// </summary>
public sealed class PathMetadataAssemblyResolver : IMetadataAssemblyResolver
{
    private readonly ConcurrentDictionary<string, string> _assemblyPaths;

    public PathMetadataAssemblyResolver(IEnumerable<string> searchPaths)
    {
        if (searchPaths is null)
        {
            throw new ArgumentNullException(nameof(searchPaths));
        }

        _assemblyPaths = new ConcurrentDictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (var path in searchPaths)
        {
            if (string.IsNullOrWhiteSpace(path))
            {
                continue;
            }

            if (Directory.Exists(path))
            {
                IndexDirectory(path);
            }
            else if (File.Exists(path))
            {
                IndexAssembly(path);
            }
        }
    }

    public bool TryResolve(string assemblyName, [NotNullWhen(true)] out MetadataResolutionResult? result)
    {
        if (string.IsNullOrWhiteSpace(assemblyName))
        {
            result = null;
            return false;
        }

        var simpleName = new AssemblyName(assemblyName).Name;
        if (simpleName is not null && _assemblyPaths.TryGetValue(simpleName, out var path))
        {
            result = CreateResult(path);
            return true;
        }

        result = null;
        return false;
    }

    public bool TryResolve(MetadataReader reader, AssemblyReferenceHandle handle, [NotNullWhen(true)] out MetadataResolutionResult? result)
    {
        var reference = reader.GetAssemblyReference(handle);
        var name = reader.GetString(reference.Name);
        return TryResolve(name, out result);
    }

    private void IndexDirectory(string directory)
    {
        foreach (var file in Directory.EnumerateFiles(directory, "*.dll"))
        {
            IndexAssembly(file);
        }
    }

    private void IndexAssembly(string path)
    {
        var name = Path.GetFileNameWithoutExtension(path);
        if (!string.IsNullOrEmpty(name))
        {
            _assemblyPaths.TryAdd(name, path);
        }
    }

    private static MetadataResolutionResult CreateResult(string path)
    {
        using var stream = File.OpenRead(path);
        using var peReader = new PEReader(stream);
        if (!peReader.HasMetadata)
        {
            throw new BadImageFormatException($"Assembly '{path}' does not contain metadata.");
        }

        var metadata = peReader.GetMetadata().GetContent();
        var provider = MetadataReaderProvider.FromMetadataImage(metadata);
        return new MetadataResolutionResult(provider, path);
    }
}
