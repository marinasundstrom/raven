using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using System.Threading;

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
        : this(CreateAssemblyPluginFactory(assembly),
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
            CreateFilePluginFactory(fullPath),
            fullPath,
            sourceProjectFilePath);
    }

    /// <summary>
    /// Creates a macro reference from an emitted managed assembly image.
    /// </summary>
    /// <param name="assemblyImage">The complete portable executable image.</param>
    /// <param name="display">An optional display name used in diagnostics.</param>
    /// <returns>A lazily loaded macro reference.</returns>
    public static MacroReference CreateFromImage(byte[] assemblyImage, string? display = null)
    {
        ArgumentNullException.ThrowIfNull(assemblyImage);
        if (assemblyImage.Length == 0)
            throw new ArgumentException("Assembly image must not be empty.", nameof(assemblyImage));

        var image = (byte[])assemblyImage.Clone();
        return new MacroReference(
            CreateImagePluginFactory(image),
            string.IsNullOrWhiteSpace(display) ? "<in-memory macro assembly>" : display,
            sourceProjectFilePath: null);
    }

    public IEnumerable<IRavenMacroPlugin> GetPlugins() => _pluginFactory();

    private static Func<IEnumerable<IRavenMacroPlugin>> CreateAssemblyPluginFactory(Assembly assembly)
    {
        var pluginTypes = new Lazy<Type[]>(
            () => GetPluginTypes(assembly),
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => pluginTypes.Value.Select(static t => (IRavenMacroPlugin)Activator.CreateInstance(t)!);
    }

    private static Func<IEnumerable<IRavenMacroPlugin>> CreateFilePluginFactory(string fullPath)
    {
        var pluginTypes = new Lazy<Type[]>(
            () =>
            {
                var loadContext = new MacroAssemblyLoadContext(fullPath);
                var assembly = loadContext.LoadFromAssemblyPath(fullPath);
                return assembly.GetTypes()
                    .Where(static t => typeof(IRavenMacroPlugin).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) is not null)
                    .ToArray();
            },
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => pluginTypes.Value.Select(static t => (IRavenMacroPlugin)Activator.CreateInstance(t)!);
    }

    private static Func<IEnumerable<IRavenMacroPlugin>> CreateImagePluginFactory(byte[] assemblyImage)
    {
        var pluginTypes = new Lazy<Type[]>(
            () =>
            {
                var loadContext = new MacroAssemblyLoadContext();
                using var stream = new MemoryStream(assemblyImage, writable: false);
                var assembly = loadContext.LoadFromStream(stream);
                return GetPluginTypes(assembly);
            },
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => pluginTypes.Value.Select(static t => (IRavenMacroPlugin)Activator.CreateInstance(t)!);
    }

    private static Type[] GetPluginTypes(Assembly assembly)
        => assembly.GetTypes()
            .Where(static t => typeof(IRavenMacroPlugin).IsAssignableFrom(t) && !t.IsAbstract && t.GetConstructor(Type.EmptyTypes) is not null)
            .ToArray();

    private sealed class MacroAssemblyLoadContext : AssemblyLoadContext
    {
        private static readonly Assembly s_macroContractsAssembly = typeof(IRavenMacroPlugin).Assembly;
        private readonly AssemblyDependencyResolver? _resolver;

        public MacroAssemblyLoadContext()
            : base($"RavenMacro:InMemory:{Guid.NewGuid():N}", isCollectible: true)
        {
        }

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

            var assemblyPath = _resolver?.ResolveAssemblyToPath(assemblyName);
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
