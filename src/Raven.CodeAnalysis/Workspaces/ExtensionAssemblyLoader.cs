using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.Loader;

namespace Raven.CodeAnalysis;

internal static class ExtensionAssemblyLoader
{
    private static readonly ConcurrentDictionary<string, Lazy<Assembly>> s_assemblies =
        new(StringComparer.OrdinalIgnoreCase);

    public static Assembly LoadFromPath(string path)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(path);

        var fullPath = Path.GetFullPath(path);
        return s_assemblies.GetOrAdd(
            fullPath,
            static assemblyPath => new Lazy<Assembly>(
                () => new ExtensionLoadContext(assemblyPath).LoadExtension(),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;
    }

    private sealed class ExtensionLoadContext : AssemblyLoadContext
    {
        private static readonly Assembly s_codeAnalysisAssembly = typeof(Compilation).Assembly;
        private static readonly string s_codeAnalysisAssemblyName =
            s_codeAnalysisAssembly.GetName().Name!;

        private readonly string _extensionPath;
        private readonly AssemblyDependencyResolver _resolver;

        public ExtensionLoadContext(string extensionPath)
            : base($"Raven extension: {extensionPath}", isCollectible: false)
        {
            _extensionPath = extensionPath;
            _resolver = new AssemblyDependencyResolver(extensionPath);
        }

        public Assembly LoadExtension() => LoadFromAssemblyPath(_extensionPath);

        protected override Assembly? Load(AssemblyName assemblyName)
        {
            if (string.Equals(assemblyName.Name, s_codeAnalysisAssemblyName, StringComparison.OrdinalIgnoreCase))
                return s_codeAnalysisAssembly;

            var dependencyPath = _resolver.ResolveAssemblyToPath(assemblyName);
            return dependencyPath is null
                ? null
                : LoadFromAssemblyPath(dependencyPath);
        }
    }
}
