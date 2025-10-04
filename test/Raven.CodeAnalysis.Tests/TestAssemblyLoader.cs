using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests;

internal static class TestAssemblyLoader
{
    internal sealed class LoadedAssembly : IDisposable
    {
        private readonly AssemblyLoadContext _context;
        public Assembly Assembly { get; }

        public LoadedAssembly(Assembly assembly, AssemblyLoadContext context)
        {
            Assembly = assembly;
            _context = context;
        }

        public void Dispose() => _context.Unload();
    }

    public static LoadedAssembly LoadFromStream(Stream peStream, IEnumerable<MetadataReference> references)
    {
        peStream.Position = 0;

        var refPaths = references
            .OfType<PortableExecutableReference>()
            .Select(r => r.FilePath)
            .Where(p => p is not null)
            .ToArray();

        var alc = new AssemblyLoadContext("RavenTests", isCollectible: true);
        alc.Resolving += (context, name) =>
        {
            try
            {
                return AssemblyLoadContext.Default.LoadFromAssemblyName(name);
            }
            catch
            {
                // ignored – fall back to explicit probing below.
            }

            var candidate = refPaths.FirstOrDefault(p =>
                string.Equals(Path.GetFileNameWithoutExtension(p), name.Name, StringComparison.OrdinalIgnoreCase));
            return candidate is not null ? context.LoadFromAssemblyPath(candidate) : null;
        };

        var assembly = alc.LoadFromStream(peStream);
        return new LoadedAssembly(assembly, alc);
    }
}

