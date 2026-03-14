using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;

namespace Raven.CodeAnalysis.Macros;

public sealed class MacroReference
{
    private readonly Func<IEnumerable<IRavenMacroPlugin>> _pluginFactory;
    private readonly string? _display;
    private readonly string? _sourceProjectFilePath;

    public MacroReference(IRavenMacroPlugin plugin)
        : this(() => [plugin], plugin.GetType().Assembly.FullName, sourceProjectFilePath: null)
    {
    }

    public MacroReference(Type pluginType)
        : this(() => [(IRavenMacroPlugin)Activator.CreateInstance(pluginType)!], pluginType.Assembly.FullName, sourceProjectFilePath: null)
    {
        if (!typeof(IRavenMacroPlugin).IsAssignableFrom(pluginType))
            throw new ArgumentException("Type must implement IRavenMacroPlugin", nameof(pluginType));
    }

    public MacroReference(Assembly assembly)
        : this(() =>
            assembly.GetTypes()
                .Where(static t => typeof(IRavenMacroPlugin).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) is not null)
                .Select(static t => (IRavenMacroPlugin)Activator.CreateInstance(t)!),
            assembly.Location,
            sourceProjectFilePath: null)
    {
    }

    private MacroReference(Func<IEnumerable<IRavenMacroPlugin>> pluginFactory, string? display, string? sourceProjectFilePath)
    {
        _pluginFactory = pluginFactory ?? throw new ArgumentNullException(nameof(pluginFactory));
        _display = display;
        _sourceProjectFilePath = string.IsNullOrWhiteSpace(sourceProjectFilePath)
            ? null
            : Path.GetFullPath(sourceProjectFilePath);
    }

    public string Display => _display ?? "<macro-reference>";
    public string? SourceProjectFilePath => _sourceProjectFilePath;

    public static MacroReference CreateFromFile(string assemblyPath, string? sourceProjectFilePath = null)
    {
        if (string.IsNullOrWhiteSpace(assemblyPath))
            throw new ArgumentException("Assembly path is required.", nameof(assemblyPath));

        var fullPath = Path.GetFullPath(assemblyPath);
        return new MacroReference(
            () =>
            {
                var loadContext = new MacroAssemblyLoadContext(fullPath);
                var assembly = loadContext.LoadFromAssemblyPath(fullPath);
                return assembly.GetTypes()
                    .Where(static t => typeof(IRavenMacroPlugin).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) is not null)
                    .Select(static t => (IRavenMacroPlugin)Activator.CreateInstance(t)!);
            },
            fullPath,
            sourceProjectFilePath);
    }

    public IEnumerable<IRavenMacroPlugin> GetPlugins() => _pluginFactory();

    private sealed class MacroAssemblyLoadContext : AssemblyLoadContext
    {
        private static readonly Assembly s_macroContractsAssembly = typeof(IRavenMacroPlugin).Assembly;
        private readonly AssemblyDependencyResolver _resolver;

        public MacroAssemblyLoadContext(string mainAssemblyPath)
            : base($"RavenMacro:{Path.GetFileNameWithoutExtension(mainAssemblyPath)}:{Guid.NewGuid():N}", isCollectible: true)
        {
            _resolver = new AssemblyDependencyResolver(mainAssemblyPath);
        }

        protected override Assembly? Load(AssemblyName assemblyName)
        {
            var sharedAssembly = TryLoadSharedAssembly(assemblyName);
            if (sharedAssembly is not null)
                return sharedAssembly;

            var assemblyPath = _resolver.ResolveAssemblyToPath(assemblyName);
            if (!string.IsNullOrWhiteSpace(assemblyPath))
                return LoadFromAssemblyPath(assemblyPath);

            return null;
        }

        private static Assembly? TryLoadSharedAssembly(AssemblyName assemblyName)
        {
            if (AssemblyName.ReferenceMatchesDefinition(assemblyName, s_macroContractsAssembly.GetName()))
                return s_macroContractsAssembly;

            try
            {
                if (IsFrameworkAssembly(assemblyName))
                    return AssemblyLoadContext.Default.LoadFromAssemblyName(assemblyName);
            }
            catch
            {
            }

            return null;
        }

        private static bool IsFrameworkAssembly(AssemblyName assemblyName)
        {
            var name = assemblyName.Name;
            if (string.IsNullOrWhiteSpace(name))
                return false;

            return name.Equals("System", StringComparison.Ordinal)
                || name.Equals("mscorlib", StringComparison.Ordinal)
                || name.Equals("netstandard", StringComparison.Ordinal)
                || name.StartsWith("System.", StringComparison.Ordinal)
                || name.StartsWith("Microsoft.", StringComparison.Ordinal);
        }
    }
}
