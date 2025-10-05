using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.IO;

using MetadataOnlyLoadContext = System.Reflection2.MetadataLoadContext;
using MetadataPathResolver = System.Reflection2.PathMetadataAssemblyResolver;
using MetadataResolutionResult = System.Reflection2.MetadataResolutionResult;
using RuntimeMetadataLoadContext = System.Reflection.MetadataLoadContext;

namespace Raven.CodeAnalysis;

internal interface IMetadataReferenceHost : IDisposable
{
    Assembly CoreAssembly { get; }

    Assembly LoadFromAssemblyPath(string path);

    Assembly? LoadFromAssemblyName(AssemblyName name);
}

internal static class MetadataReferenceHost
{
    public static IMetadataReferenceHost Create(MetadataReferenceHostKind kind, IReadOnlyCollection<string> referencePaths)
        => kind switch
        {
            MetadataReferenceHostKind.SystemReflection2 => new SystemReflection2Host(referencePaths),
            _ => new RuntimeMetadataHost(referencePaths),
        };

    private sealed class RuntimeMetadataHost : IMetadataReferenceHost
    {
        private readonly RuntimeMetadataLoadContext _context;
        private readonly Dictionary<string, Assembly> _pathCache = new(StringComparer.OrdinalIgnoreCase);

        public RuntimeMetadataHost(IEnumerable<string> referencePaths)
        {
            var resolver = new PathAssemblyResolver(referencePaths);
            _context = new RuntimeMetadataLoadContext(resolver);
        }

        public Assembly CoreAssembly => _context.CoreAssembly!;

        public Assembly LoadFromAssemblyPath(string path)
        {
            if (_pathCache.TryGetValue(path, out var assembly))
            {
                return assembly;
            }

            assembly = _context.LoadFromAssemblyPath(path);
            _pathCache[path] = assembly;
            return assembly;
        }

        public Assembly? LoadFromAssemblyName(AssemblyName name)
        {
            try
            {
                return _context.LoadFromAssemblyName(name);
            }
            catch
            {
                return null;
            }
        }

        public void Dispose() => _context.Dispose();
    }

    private sealed class SystemReflection2Host : IMetadataReferenceHost
    {
        private readonly MetadataOnlyLoadContext _context;
        private readonly MetadataPathResolver _resolver;
        private readonly Dictionary<string, Assembly> _pathCache = new(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, Assembly> _nameCache = new(StringComparer.OrdinalIgnoreCase);
        private readonly IReadOnlyCollection<string> _referencePaths;
        private Assembly? _coreAssembly;

        public SystemReflection2Host(IReadOnlyCollection<string> referencePaths)
        {
            _referencePaths = referencePaths;
            _resolver = new MetadataPathResolver(referencePaths);
            _context = new MetadataOnlyLoadContext(_resolver);
        }

        public Assembly CoreAssembly => _coreAssembly ??= ResolveCoreAssembly();

        public Assembly LoadFromAssemblyPath(string path)
        {
            if (string.IsNullOrEmpty(path))
            {
                throw new FileNotFoundException("Metadata reference path is null or empty.");
            }

            if (_pathCache.TryGetValue(path, out var existing))
            {
                return existing;
            }

            if (!File.Exists(path))
            {
                throw new FileNotFoundException($"Metadata reference '{path}' does not exist.", path);
            }

            var result = CreateResolutionResult(path);
            var assembly = _context.RegisterAssembly(result);
            CacheAssembly(assembly, path);
            return assembly;
        }

        public Assembly? LoadFromAssemblyName(AssemblyName name)
        {
            if (name.Name is null)
            {
                return null;
            }

            if (_nameCache.TryGetValue(name.Name, out var existing))
            {
                return existing;
            }

            MetadataResolutionResult? result = null;
            if (!string.IsNullOrEmpty(name.FullName))
            {
                _resolver.TryResolve(name.FullName, out result);
            }

            if (result is null)
            {
                _resolver.TryResolve(name.Name, out result);
            }

            if (result is null)
            {
                return null;
            }

            var assembly = _context.RegisterAssembly(result);
            CacheAssembly(assembly, result.Location);
            return assembly;
        }

        public void Dispose() => _context.Dispose();

        private Assembly ResolveCoreAssembly()
        {
            foreach (var path in _referencePaths)
            {
                if (string.IsNullOrEmpty(path))
                {
                    continue;
                }

                try
                {
                    var assembly = LoadFromAssemblyPath(path);
                    if (assembly.GetType("System.Object", throwOnError: false) is not null)
                    {
                        return assembly;
                    }
                }
                catch
                {
                    // Ignore and fall back to the runtime core assembly if none resolve.
                }
            }

            return typeof(object).Assembly;
        }

        private static MetadataResolutionResult CreateResolutionResult(string path)
        {
            using var stream = File.OpenRead(path);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
            {
                throw new BadImageFormatException($"Metadata reference '{path}' does not contain metadata.");
            }

            var metadata = peReader.GetMetadata().GetContent();
            var provider = MetadataReaderProvider.FromMetadataImage(metadata);
            return new MetadataResolutionResult(provider, path);
        }

        private void CacheAssembly(Assembly assembly, string? path)
        {
            if (!string.IsNullOrEmpty(path))
            {
                _pathCache[path] = assembly;
            }

            var name = assembly.GetName();
            if (!string.IsNullOrEmpty(name.Name))
            {
                _nameCache[name.Name] = assembly;
            }

            if (!string.IsNullOrEmpty(name.FullName))
            {
                _nameCache[name.FullName] = assembly;
            }
        }
    }
}
