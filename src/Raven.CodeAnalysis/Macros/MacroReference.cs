using System;
using System.Collections.Immutable;
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

    public MacroReference(IMacroDefinition macro)
        : this(
            () => [new ManifestMacroPlugin(macro.GetType().Assembly.GetName().Name, [macro])],
            macro.GetType().Assembly.FullName,
            sourceProjectFilePath: null)
    {
    }

    public MacroReference(Type exportedType)
        : this(CreateExportedTypeFactory(exportedType), exportedType.Assembly.FullName, sourceProjectFilePath: null)
    {
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
        var exports = new Lazy<MacroAssemblyExports>(
            () => GetExports(assembly),
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => exports.Value.CreatePlugins();
    }

    private static Func<IEnumerable<IRavenMacroPlugin>> CreateFilePluginFactory(string fullPath)
    {
        var exports = new Lazy<MacroAssemblyExports>(
            () =>
            {
                var loadContext = new MacroAssemblyLoadContext(fullPath);
                var assembly = loadContext.LoadFromAssemblyPath(fullPath);
                return GetExports(assembly);
            },
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => exports.Value.CreatePlugins();
    }

    private static Func<IEnumerable<IRavenMacroPlugin>> CreateImagePluginFactory(byte[] assemblyImage)
    {
        var exports = new Lazy<MacroAssemblyExports>(
            () =>
            {
                var loadContext = new MacroAssemblyLoadContext();
                using var stream = new MemoryStream(assemblyImage, writable: false);
                var assembly = loadContext.LoadFromStream(stream);
                return GetExports(assembly);
            },
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => exports.Value.CreatePlugins();
    }

    private static Func<IEnumerable<IRavenMacroPlugin>> CreateExportedTypeFactory(Type exportedType)
    {
        ArgumentNullException.ThrowIfNull(exportedType);
        if (!IsConstructibleExportedType(exportedType))
        {
            throw new ArgumentException(
                $"Macro export '{exportedType.FullName}' must be a non-abstract class that implements {nameof(IMacroDefinition)} or {nameof(IRavenMacroPlugin)} and has a public parameterless constructor.",
                nameof(exportedType));
        }

        if (typeof(IRavenMacroPlugin).IsAssignableFrom(exportedType))
            return () => [(IRavenMacroPlugin)Activator.CreateInstance(exportedType)!];

        return () =>
        [
            new ManifestMacroPlugin(
                exportedType.Assembly.GetName().Name,
                [(IMacroDefinition)Activator.CreateInstance(exportedType)!])
        ];
    }

    private static MacroAssemblyExports GetExports(Assembly assembly)
    {
        var markers = assembly.GetCustomAttributes<RavenCompilerPluginAttribute>().ToArray();
        var declaredTypes = markers
            .Select(static marker => marker.ExportedType)
            .Where(static type => type is not null)
            .Cast<Type>()
            .ToArray();

        if (declaredTypes.Length > 0)
        {
            if (markers.Any(static marker => marker.ExportedType is null))
            {
                throw new InvalidOperationException(
                    $"Compiler plugin assembly '{assembly.GetName().Name}' mixes explicit entry points with the fallback-discovery marker.");
            }

            foreach (var declaredType in declaredTypes)
                ValidateExportedType(assembly, declaredType);

            return new MacroAssemblyExports(
                assembly.GetName().Name,
                declaredTypes
                    .Where(static type => typeof(IRavenMacroPlugin).IsAssignableFrom(type))
                    .Distinct()
                    .ToArray(),
                declaredTypes
                    .Where(static type => typeof(IMacroDefinition).IsAssignableFrom(type))
                    .Distinct()
                    .ToArray());
        }

        var exportedTypes = assembly.GetTypes();
        var pluginTypes = exportedTypes
            .Where(IsConstructiblePluginType)
            .ToArray();

        // A legacy plugin owns macro aggregation when one is present. Otherwise,
        // the assembly is a direct macro partition and its definitions are the
        // exports. This prevents definitions returned by a compatibility plugin
        // from also being registered independently.
        return new MacroAssemblyExports(
            assembly.GetName().Name,
            pluginTypes,
            pluginTypes.Length > 0
                ? []
                : exportedTypes
                    .Where(static type =>
                        IsConstructibleExportedType(type) &&
                        typeof(IMacroDefinition).IsAssignableFrom(type))
                    .ToArray());
    }

    private static void ValidateExportedType(Assembly assembly, Type exportedType)
    {
        if (exportedType.Assembly != assembly)
        {
            throw new InvalidOperationException(
                $"Compiler plugin export '{exportedType.FullName}' must be declared in assembly '{assembly.GetName().Name}'.");
        }

        if (!IsConstructibleExportedType(exportedType))
        {
            throw new InvalidOperationException(
                $"Compiler plugin export '{exportedType.FullName}' must be a non-abstract class that implements {nameof(IMacroDefinition)} or {nameof(IRavenMacroPlugin)} and has a public parameterless constructor.");
        }
    }

    private static bool IsConstructibleExportedType(Type type)
        => type.IsClass
            && (typeof(IMacroDefinition).IsAssignableFrom(type) ||
                typeof(IRavenMacroPlugin).IsAssignableFrom(type))
            && !type.IsAbstract
            && !type.ContainsGenericParameters
            && type.GetConstructor(Type.EmptyTypes) is not null;

    private static bool IsConstructiblePluginType(Type type)
        => type.IsClass
            && typeof(IRavenMacroPlugin).IsAssignableFrom(type)
            && !type.IsAbstract
            && !type.ContainsGenericParameters
            && type.GetConstructor(Type.EmptyTypes) is not null;

    private sealed record MacroAssemblyExports(
        string? AssemblyName,
        Type[] PluginTypes,
        Type[] MacroTypes)
    {
        public IEnumerable<IRavenMacroPlugin> CreatePlugins()
        {
            foreach (var pluginType in PluginTypes)
                yield return (IRavenMacroPlugin)Activator.CreateInstance(pluginType)!;

            if (MacroTypes.Length > 0)
            {
                yield return new ManifestMacroPlugin(
                    AssemblyName,
                    MacroTypes
                        .Select(static type => (IMacroDefinition)Activator.CreateInstance(type)!)
                        .ToImmutableArray());
            }
        }
    }

    private sealed class ManifestMacroPlugin : IRavenMacroPlugin
    {
        private readonly ImmutableArray<IMacroDefinition> _macros;

        public ManifestMacroPlugin(string? assemblyName, ImmutableArray<IMacroDefinition> macros)
        {
            Name = string.IsNullOrWhiteSpace(assemblyName)
                ? "<manifest macros>"
                : assemblyName;
            _macros = macros;
        }

        public string Name { get; }

        public ImmutableArray<IMacroDefinition> GetMacros() => _macros;
    }

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
