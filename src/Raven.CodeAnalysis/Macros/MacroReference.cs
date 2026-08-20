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
    private readonly Lazy<MacroSnapshot> _snapshot;
    private readonly string? _display;
    private readonly string? _sourceProjectFilePath;

    public MacroReference(IMacroDefinition macro)
        : this(
            () => new MacroSnapshot([macro], LoadContext: null),
            macro.GetType().Assembly.FullName,
            sourceProjectFilePath: null)
    {
    }

    public MacroReference(Type exportedType)
        : this(CreateExportedTypeFactory(exportedType), exportedType.Assembly.FullName, sourceProjectFilePath: null)
    {
    }

    public MacroReference(Assembly assembly)
        : this(CreateAssemblyMacroFactory(assembly),
            assembly.Location,
            sourceProjectFilePath: null)
    {
    }

    /// <summary>
    /// Creates a macro reference from an already loaded assembly.
    /// </summary>
    /// <param name="assembly">The loaded compiler-plugin assembly.</param>
    /// <param name="display">
    /// An optional path or label used for diagnostics and duplicate-reference
    /// detection.
    /// </param>
    public static MacroReference CreateFromAssembly(
        Assembly assembly,
        string? display = null)
    {
        ArgumentNullException.ThrowIfNull(assembly);
        return new MacroReference(
            CreateAssemblyMacroFactory(assembly),
            display ?? assembly.Location,
            sourceProjectFilePath: null);
    }

    private MacroReference(Func<MacroSnapshot> macroFactory, string? display, string? sourceProjectFilePath)
    {
        ArgumentNullException.ThrowIfNull(macroFactory);
        _snapshot = new Lazy<MacroSnapshot>(
            macroFactory,
            LazyThreadSafetyMode.ExecutionAndPublication);
        _display = display;
        _sourceProjectFilePath = string.IsNullOrWhiteSpace(sourceProjectFilePath)
            ? null
            : Path.GetFullPath(sourceProjectFilePath);
    }

    public string Display => _display ?? "<macro-reference>";
    public string? SourceProjectFilePath => _sourceProjectFilePath;

    public static MacroReference CreateFromFile(
        string assemblyPath,
        string? sourceProjectFilePath = null)
        => CreateFromFile(
            assemblyPath,
            sourceProjectFilePath,
            dependencyAssemblyPaths: null);

    internal static MacroReference CreateFromFile(
        string assemblyPath,
        string? sourceProjectFilePath,
        IEnumerable<string>? dependencyAssemblyPaths)
    {
        if (string.IsNullOrWhiteSpace(assemblyPath))
            throw new ArgumentException("Assembly path is required.", nameof(assemblyPath));

        var fullPath = Path.GetFullPath(assemblyPath);
        var dependencies = dependencyAssemblyPaths?
            .Where(static path => !string.IsNullOrWhiteSpace(path))
            .Select(Path.GetFullPath)
            .Where(path => !string.Equals(path, fullPath, StringComparison.OrdinalIgnoreCase))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray() ?? ImmutableArray<string>.Empty;
        return new MacroReference(
            CreateFileMacroFactory(fullPath, dependencies),
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
            CreateImageMacroFactory(image),
            string.IsNullOrWhiteSpace(display) ? "<in-memory macro assembly>" : display,
            sourceProjectFilePath: null);
    }

    /// <summary>
    /// Gets the immutable macro-definition snapshot exported by this reference.
    /// </summary>
    public ImmutableArray<IMacroDefinition> Macros => _snapshot.Value.Macros;

    private static Func<MacroSnapshot> CreateAssemblyMacroFactory(Assembly assembly)
    {
        var exports = new Lazy<MacroAssemblyExports>(
            () => GetExports(assembly),
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => exports.Value.CreateSnapshot();
    }

    private static Func<MacroSnapshot> CreateFileMacroFactory(
        string fullPath,
        ImmutableArray<string> dependencyAssemblyPaths)
    {
        var exports = new Lazy<MacroAssemblyExports>(
            () =>
            {
                var loadContext = new MacroAssemblyLoadContext(
                    fullPath,
                    dependencyAssemblyPaths);
                var assembly = loadContext.LoadFromAssemblyPath(fullPath);
                return GetExports(assembly, loadContext);
            },
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => exports.Value.CreateSnapshot();
    }

    private static Func<MacroSnapshot> CreateImageMacroFactory(byte[] assemblyImage)
    {
        var exports = new Lazy<MacroAssemblyExports>(
            () =>
            {
                var loadContext = new MacroAssemblyLoadContext();
                using var stream = new MemoryStream(assemblyImage, writable: false);
                var assembly = loadContext.LoadFromStream(stream);
                return GetExports(assembly, loadContext);
            },
            LazyThreadSafetyMode.ExecutionAndPublication);

        return () => exports.Value.CreateSnapshot();
    }

    private static Func<MacroSnapshot> CreateExportedTypeFactory(Type exportedType)
    {
        ArgumentNullException.ThrowIfNull(exportedType);
        if (!IsConstructibleExportedType(exportedType))
        {
            throw new ArgumentException(
                $"Macro export '{exportedType.FullName}' must be a non-abstract macro class with one supported Expand contract and a public parameterless constructor.",
                nameof(exportedType));
        }

        return () => new MacroSnapshot(
            [CreateMacroInstance(exportedType)],
            LoadContext: null);
    }

    private static IMacroDefinition CreateMacroInstance(Type macroType)
    {
        try
        {
            return (IMacroDefinition)Activator.CreateInstance(macroType)!;
        }
        catch (Exception exception)
        {
            var failure = exception;
            while (failure is TargetInvocationException { InnerException: not null } invocationException)
                failure = invocationException.InnerException;

            var detail = string.IsNullOrWhiteSpace(failure.Message)
                ? failure.GetType().Name
                : failure.Message;
            throw new InvalidOperationException(
                $"Macro provider '{macroType.FullName ?? macroType.Name}' could not be created: {detail}",
                failure);
        }
    }

    private static MacroAssemblyExports GetExports(
        Assembly assembly,
        AssemblyLoadContext? loadContext = null)
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
                declaredTypes
                    .Where(static type => typeof(IMacroDefinition).IsAssignableFrom(type))
                    .Distinct()
                    .ToArray(),
                loadContext);
        }

        var exportedTypes = assembly.GetTypes();
        if (markers.Length > 0)
            exportedTypes = exportedTypes.Where(static type => type.IsPublic).ToArray();

        return new MacroAssemblyExports(
            exportedTypes
                .Where(IsConstructibleExportedType)
                .ToArray(),
            loadContext);
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
                $"Compiler plugin export '{exportedType.FullName}' must be a non-abstract macro class with one supported Expand contract and a public parameterless constructor.");
        }
    }

    private static bool IsConstructibleExportedType(Type type)
        => type.IsClass
            && typeof(IMacroDefinition).IsAssignableFrom(type)
            && HasExactlyOneMacroRole(type)
            && !type.IsAbstract
            && !type.ContainsGenericParameters
            && type.GetConstructor(Type.EmptyTypes) is not null;

    private static bool HasExactlyOneMacroRole(Type type)
        => (typeof(IMacroExecutor).IsAssignableFrom(type) ? 1 : 0) +
            (typeof(IAttachedDeclarationMacro).IsAssignableFrom(type) ? 1 : 0) +
            (typeof(IInvocableMacro).IsAssignableFrom(type) ? 1 : 0) +
            (typeof(ITokenTreeMacro).IsAssignableFrom(type) ? 1 : 0) +
            (MethodMacroFacts.TryGetExpandMethod(type, out _) ? 1 : 0) == 1;

    private sealed record MacroAssemblyExports(
        Type[] MacroTypes,
        AssemblyLoadContext? LoadContext)
    {
        public MacroSnapshot CreateSnapshot()
            => new(
                MacroTypes
                    .Select(CreateMacroInstance)
                    .ToImmutableArray(),
                LoadContext);
    }

    private sealed record MacroSnapshot(
        ImmutableArray<IMacroDefinition> Macros,
        AssemblyLoadContext? LoadContext);

    private sealed class MacroAssemblyLoadContext : AssemblyLoadContext
    {
        private static readonly Assembly s_macroContractsAssembly = typeof(IMacroDefinition).Assembly;
        private readonly AssemblyDependencyResolver? _resolver;
        private readonly string? _mainAssemblyDirectory;
        private readonly ImmutableArray<string> _dependencyAssemblyPaths;
        private readonly object _loadGate = new();

        public MacroAssemblyLoadContext()
            : base($"RavenMacro:InMemory:{Guid.NewGuid():N}", isCollectible: true)
        {
        }

        public MacroAssemblyLoadContext(
            string mainAssemblyPath,
            ImmutableArray<string> dependencyAssemblyPaths)
            : base($"RavenMacro:{Path.GetFileNameWithoutExtension(mainAssemblyPath)}:{Guid.NewGuid():N}", isCollectible: true)
        {
            _resolver = new AssemblyDependencyResolver(mainAssemblyPath);
            _mainAssemblyDirectory = Path.GetDirectoryName(mainAssemblyPath);
            _dependencyAssemblyPaths = dependencyAssemblyPaths;
        }

        protected override Assembly? Load(AssemblyName assemblyName)
        {
            var sharedAssembly = TryLoadSharedAssembly(assemblyName);
            if (sharedAssembly is not null)
                return sharedAssembly;

            lock (_loadGate)
            {
                var loadedAssembly = Assemblies.FirstOrDefault(
                    assembly => AssemblyName.ReferenceMatchesDefinition(
                        assemblyName,
                        assembly.GetName()));
                if (loadedAssembly is not null)
                    return loadedAssembly;

                return LoadDependency(assemblyName);
            }
        }

        private Assembly? LoadDependency(AssemblyName assemblyName)
        {
            var assemblyPath = _resolver?.ResolveAssemblyToPath(assemblyName);
            if (!string.IsNullOrWhiteSpace(assemblyPath))
                return LoadFromAssemblyPath(assemblyPath);

            if (!string.IsNullOrWhiteSpace(_mainAssemblyDirectory) &&
                !string.IsNullOrWhiteSpace(assemblyName.Name))
            {
                var adjacentAssemblyPath = Path.Combine(
                    _mainAssemblyDirectory,
                    $"{assemblyName.Name}.dll");
                if (File.Exists(adjacentAssemblyPath) &&
                    AssemblyName.ReferenceMatchesDefinition(
                        assemblyName,
                        AssemblyName.GetAssemblyName(adjacentAssemblyPath)))
                {
                    return LoadFromAssemblyPath(adjacentAssemblyPath);
                }
            }

            foreach (var dependencyAssemblyPath in _dependencyAssemblyPaths)
            {
                if (!string.Equals(
                        Path.GetFileNameWithoutExtension(dependencyAssemblyPath),
                        assemblyName.Name,
                        StringComparison.OrdinalIgnoreCase) ||
                    !File.Exists(dependencyAssemblyPath))
                {
                    continue;
                }

                try
                {
                    if (AssemblyName.ReferenceMatchesDefinition(
                        assemblyName,
                        AssemblyName.GetAssemblyName(dependencyAssemblyPath)))
                    {
                        return LoadFromAssemblyPath(dependencyAssemblyPath);
                    }
                }
                catch (Exception exception) when (
                    exception is BadImageFormatException or
                        FileLoadException or
                        FileNotFoundException)
                {
                }
            }

            return null;
        }

        private static Assembly? TryLoadSharedAssembly(AssemblyName assemblyName)
        {
            var contractsAssemblyName = s_macroContractsAssembly.GetName();
            if (string.Equals(
                    assemblyName.Name,
                    contractsAssemblyName.Name,
                    StringComparison.OrdinalIgnoreCase))
            {
                if (!AssemblyName.ReferenceMatchesDefinition(
                        assemblyName,
                        contractsAssemblyName) ||
                    assemblyName.Version is not null &&
                    contractsAssemblyName.Version is not null &&
                    assemblyName.Version != contractsAssemblyName.Version)
                {
                    throw new FileLoadException(
                        $"Macro provider requires '{assemblyName.FullName}', but the compiler host uses '{contractsAssemblyName.FullName}'. Rebuild the macro library against a compatible Raven.CodeAnalysis version.");
                }

                return s_macroContractsAssembly;
            }

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
