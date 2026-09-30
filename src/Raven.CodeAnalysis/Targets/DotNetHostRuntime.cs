using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using System.Threading;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Targets;

// Host execution services, owned by one compilation's .NET target. Only the
// existing process-wide assembly/path caches are shared; no semantic symbols or
// compilation references are retained here.
internal sealed class DotNetHostRuntime
{
    private readonly ConcurrentDictionary<string, string> _assemblyPathMap = new(StringComparer.OrdinalIgnoreCase);
    private readonly ConcurrentDictionary<Assembly, Assembly> _metadataToRuntimeAssemblyMap = new();
    private readonly ConcurrentDictionary<string, Assembly> _runtimeAssemblyCache = new(StringComparer.OrdinalIgnoreCase);
    private static readonly ConcurrentDictionary<string, string> s_globalAssemblyPathMap = new(StringComparer.OrdinalIgnoreCase);
    private static readonly ConcurrentDictionary<string, Assembly> s_globalRuntimeAssemblyCache = new(StringComparer.OrdinalIgnoreCase);
    private static int s_trustedPlatformAssembliesInitialized;
    private bool _trustedPlatformAssembliesCached;

    internal Assembly RuntimeCoreAssembly => typeof(object).Assembly;

    internal IEnumerable<string> GetHostMetadataAssemblyPaths()
    {
        EnsureTrustedPlatformAssembliesCached();
        return _assemblyPathMap.Values;
    }

    internal static void RegisterSharedMetadataAssemblyPath(string name, string path)
        => s_globalAssemblyPathMap[name] = path;

    internal Assembly? ResolveEmitCoreAssembly()
    {
        var loadedSystemRuntime = AppDomain.CurrentDomain
            .GetAssemblies()
            .FirstOrDefault(static assembly =>
                string.Equals(assembly.GetName().Name, "System.Runtime", StringComparison.OrdinalIgnoreCase));
        if (loadedSystemRuntime is not null)
            return loadedSystemRuntime;

        if (_assemblyPathMap.TryGetValue("System.Runtime", out var systemRuntimePath))
        {
            try
            {
                var identity = DotNetMetadataContextFactory.ReadAssemblyName(systemRuntimePath);
                var loaded = LoadRuntimeAssemblyFromPath(identity, systemRuntimePath);
                if (loaded is not null)
                    return loaded;
            }
            catch
            {
            }
        }

        try
        {
            return System.Reflection.Assembly.Load("System.Runtime");
        }
        catch
        {
            return null;
        }
    }

    internal void RegisterMetadataAssemblyPath(string name, string path)
    {
        _assemblyPathMap[name] = path;
        s_globalAssemblyPathMap[name] = path;
    }

    internal string? GetRegisteredMetadataAssemblyPath(string name)
        => _assemblyPathMap.TryGetValue(name, out var path) ? path : null;

    internal Assembly? RegisterRuntimeAssembly(Assembly metadataAssembly, string? explicitPath = null)
    {
        if (metadataAssembly is null)
            return null;

        EnsureTrustedPlatformAssembliesCached();

        var identity = metadataAssembly.GetName();
        if (!string.IsNullOrEmpty(explicitPath) && identity.Name is not null)
            _assemblyPathMap[identity.Name] = explicitPath;
        else if (identity.Name is { } identityName)
        {
            if (!_assemblyPathMap.ContainsKey(identityName) && s_globalAssemblyPathMap.TryGetValue(identityName, out var sharedPath))
                _assemblyPathMap.TryAdd(identityName, sharedPath);

            try
            {
                var metadataLocation = metadataAssembly.Location;
                if (!string.IsNullOrEmpty(metadataLocation) && !_assemblyPathMap.ContainsKey(identityName))
                {
                    _assemblyPathMap[identityName] = metadataLocation;
                }
            }
            catch (NotSupportedException)
            {
                // Dynamic assemblies can throw when querying Location.
            }
        }

        if (identity.Name is not null &&
            !_runtimeAssemblyCache.ContainsKey(identity.Name) &&
            s_globalRuntimeAssemblyCache.TryGetValue(identity.Name, out var sharedRuntimeAssembly))
        {
            _runtimeAssemblyCache.TryAdd(identity.Name, sharedRuntimeAssembly);
        }

        if (_metadataToRuntimeAssemblyMap.TryGetValue(metadataAssembly, out var cached))
            return cached;

        Assembly? runtimeAssembly = null;
        string? resolvedPath = null;

        if (identity.Name is not null)
        {
            var alreadyLoaded = AppDomain.CurrentDomain
                .GetAssemblies()
                .FirstOrDefault(a => string.Equals(a.GetName().Name, identity.Name, StringComparison.OrdinalIgnoreCase));

            if (alreadyLoaded is not null)
            {
                runtimeAssembly = alreadyLoaded;
                if (!string.IsNullOrEmpty(alreadyLoaded.Location))
                    resolvedPath = alreadyLoaded.Location;
            }
        }

        if (identity.Name is not null && _runtimeAssemblyCache.TryGetValue(identity.Name, out var fromCache))
        {
            runtimeAssembly = fromCache;
            if (!string.IsNullOrEmpty(runtimeAssembly.Location))
                resolvedPath = runtimeAssembly.Location;
        }
        else if (identity.Name is not null && _assemblyPathMap.TryGetValue(identity.Name, out var knownPath))
        {
            var runtimePath = DotNetRuntimeAssemblyPathResolver.FindImplementation(knownPath) ?? knownPath;
            runtimeAssembly = LoadRuntimeAssemblyFromPath(identity, runtimePath);
            if (runtimeAssembly is not null)
            {
                resolvedPath = !string.IsNullOrEmpty(runtimeAssembly.Location)
                    ? runtimeAssembly.Location
                    : runtimePath;
            }
            else if (!string.Equals(runtimePath, knownPath, StringComparison.OrdinalIgnoreCase) &&
                     LoadRuntimeAssemblyFromPath(identity, knownPath) is { } metadataAssemblyRuntime)
            {
                runtimeAssembly = metadataAssemblyRuntime;
                resolvedPath = !string.IsNullOrEmpty(runtimeAssembly.Location)
                    ? runtimeAssembly.Location
                    : knownPath;
            }
        }

        if (runtimeAssembly is null && !string.IsNullOrEmpty(explicitPath))
        {
            var runtimePath = DotNetRuntimeAssemblyPathResolver.FindImplementation(explicitPath) ?? explicitPath;
            runtimeAssembly = LoadRuntimeAssemblyFromPath(identity, runtimePath);
            if (runtimeAssembly is not null)
            {
                resolvedPath = !string.IsNullOrEmpty(runtimeAssembly.Location)
                    ? runtimeAssembly.Location
                    : runtimePath;
            }
            else if (!string.Equals(runtimePath, explicitPath, StringComparison.OrdinalIgnoreCase) &&
                     LoadRuntimeAssemblyFromPath(identity, explicitPath) is { } metadataAssemblyRuntime)
            {
                runtimeAssembly = metadataAssemblyRuntime;
                resolvedPath = !string.IsNullOrEmpty(runtimeAssembly.Location)
                    ? runtimeAssembly.Location
                    : explicitPath;
            }
        }

        if (runtimeAssembly is null)
        {
            runtimeAssembly = LoadRuntimeAssemblyByName(identity);
            if (runtimeAssembly is not null && !string.IsNullOrEmpty(runtimeAssembly.Location))
                resolvedPath = runtimeAssembly.Location;
        }

        if (runtimeAssembly is null)
        {
            runtimeAssembly = MapToRuntimeImplementation(identity);
            if (runtimeAssembly is not null && !string.IsNullOrEmpty(runtimeAssembly.Location))
                resolvedPath = runtimeAssembly.Location;
        }

        if (runtimeAssembly is not null)
        {
            if (identity.Name is not null)
            {
                _runtimeAssemblyCache[identity.Name] = runtimeAssembly;

                if (!string.IsNullOrEmpty(resolvedPath))
                {
                    _assemblyPathMap[identity.Name] = resolvedPath;
                    s_globalAssemblyPathMap[identity.Name] = resolvedPath;
                }
                else if (!string.IsNullOrEmpty(runtimeAssembly.Location))
                {
                    _assemblyPathMap[identity.Name] = runtimeAssembly.Location;
                    s_globalAssemblyPathMap[identity.Name] = runtimeAssembly.Location;
                }

                s_globalRuntimeAssemblyCache[identity.Name] = runtimeAssembly;
            }

            _metadataToRuntimeAssemblyMap[metadataAssembly] = runtimeAssembly;
        }

        return runtimeAssembly;
    }

    private void EnsureTrustedPlatformAssembliesCached()
    {
        if (_trustedPlatformAssembliesCached)
            return;

        if (Interlocked.CompareExchange(ref s_trustedPlatformAssembliesInitialized, 1, 0) == 0)
        {
            var platformAssemblies = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string;
            if (!string.IsNullOrWhiteSpace(platformAssemblies))
            {
                var candidates = platformAssemblies.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries);

                foreach (var candidate in candidates)
                {
                    if (string.IsNullOrWhiteSpace(candidate) || !File.Exists(candidate))
                        continue;

                    System.Reflection.AssemblyName? candidateIdentity;
                    try
                    {
                        candidateIdentity = DotNetMetadataContextFactory.ReadAssemblyName(candidate);
                    }
                    catch
                    {
                        continue;
                    }

                    if (candidateIdentity.Name is not { Length: > 0 } candidateName)
                        continue;

                    s_globalAssemblyPathMap.TryAdd(candidateName, candidate);

                    if (s_globalRuntimeAssemblyCache.ContainsKey(candidateName))
                        continue;

                    var alreadyLoaded = AppDomain.CurrentDomain
                        .GetAssemblies()
                        .FirstOrDefault(a => string.Equals(a.GetName().Name, candidateName, StringComparison.OrdinalIgnoreCase));

                    if (alreadyLoaded is not null)
                        s_globalRuntimeAssemblyCache.TryAdd(candidateName, alreadyLoaded);
                }
            }
        }

        foreach (var (assemblyName, path) in s_globalAssemblyPathMap)
            _assemblyPathMap.TryAdd(assemblyName, path);

        foreach (var (assemblyName, runtimeAssembly) in s_globalRuntimeAssemblyCache)
            _runtimeAssemblyCache.TryAdd(assemblyName, runtimeAssembly);

        _trustedPlatformAssembliesCached = true;
    }

    private Assembly? MapToRuntimeImplementation(AssemblyName identity)
    {
        if (identity.Name is null)
            return null;

        var runtimeCoreIdentity = RuntimeCoreAssembly.GetName();

        if (string.Equals(identity.Name, runtimeCoreIdentity.Name, StringComparison.OrdinalIgnoreCase))
            return RuntimeCoreAssembly;

        if (string.Equals(identity.Name, "System.Runtime", StringComparison.OrdinalIgnoreCase))
            return RuntimeCoreAssembly;

        if (string.Equals(identity.Name, "System.Private.CoreLib", StringComparison.OrdinalIgnoreCase))
            return RuntimeCoreAssembly;

        return null;
    }

    private static Assembly? LoadRuntimeAssemblyFromPath(AssemblyName identity, string? path)
    {
        if (string.IsNullOrEmpty(path))
            return null;

        try
        {
            return AssemblyLoadContext.Default.LoadFromAssemblyPath(path);
        }
        catch (FileLoadException)
        {
            return LoadRuntimeAssemblyByName(identity);
        }
        catch (BadImageFormatException)
        {
            return null;
        }
        catch (FileNotFoundException)
        {
            return LoadRuntimeAssemblyByName(identity);
        }
    }

    private static Assembly? LoadRuntimeAssemblyByName(AssemblyName identity)
    {
        try
        {
            return System.Reflection.Assembly.Load(identity);
        }
        catch
        {
            return null;
        }
    }

    internal Type? ResolveRuntimeType(PENamedTypeSymbol symbol)
    {
        if (symbol is null)
            throw new ArgumentNullException(nameof(symbol));

        var metadataName = ((INamedTypeSymbol)symbol).ToFullyQualifiedMetadataName();

        if (string.IsNullOrEmpty(metadataName))
            return null;

        if (symbol.ContainingAssembly is PEAssemblySymbol peAssembly &&
            RegisterRuntimeAssembly(peAssembly.GetAssemblyInfo()) is { } containingRuntimeAssembly &&
            GetTypeSafe(containingRuntimeAssembly, metadataName) is { } containingRuntimeType)
        {
            return containingRuntimeType;
        }

        var resolved = ResolveRuntimeType(metadataName);
        if (resolved is not null)
            return resolved;

        if (symbol.ContainingAssembly is PEAssemblySymbol peAssembly2)
        {
            if (!string.IsNullOrEmpty(peAssembly2.FullName))
            {
                var qualifiedName = $"{metadataName}, {peAssembly2.FullName}";
                var qualifiedType = GetTypeSafe(qualifiedName);
                if (qualifiedType is not null)
                {
                    RegisterRuntimeAssembly(qualifiedType.Assembly);
                    return qualifiedType;
                }
            }

            if (!string.IsNullOrEmpty(peAssembly2.Name))
            {
                try
                {
                    var assembly = System.Reflection.Assembly.Load(new AssemblyName(peAssembly2.Name));
                    RegisterRuntimeAssembly(assembly);
                    var type = GetTypeSafe(assembly, metadataName);
                    if (type is not null)
                        return type;
                }
                catch
                {
                    // Ignore load failures and fall through to null.
                }
            }
        }

        return null;
    }

    internal Type? ResolveRuntimeType(System.Reflection.TypeInfo metadataType)
    {
        if (metadataType is null)
            throw new ArgumentNullException(nameof(metadataType));

        RegisterRuntimeAssembly(metadataType.Assembly);

        if (metadataType.FullName is { Length: > 0 } fullName)
        {
            var resolved = ResolveRuntimeType(fullName);
            if (resolved is not null)
                return resolved;
        }

        if (metadataType.AssemblyQualifiedName is { Length: > 0 } qualifiedName)
            return GetTypeSafe(qualifiedName);

        return null;
    }

    internal Type? ResolveRuntimeType(string metadataName)
    {
        if (metadataName is null)
            throw new ArgumentNullException(nameof(metadataName));

        if (GetTypeSafe(RuntimeCoreAssembly, metadataName) is { } coreType)
            return coreType;

        foreach (var (key, runtimeAssembly) in _runtimeAssemblyCache)
        {
            var candidate = GetTypeSafe(runtimeAssembly, metadataName);
            if (candidate is not null)
                return candidate;
        }

        return GetTypeSafe(metadataName);
    }

    private static Type? GetTypeSafe(Assembly assembly, string metadataName)
    {
        try
        {
            return assembly.GetType(metadataName, throwOnError: false, ignoreCase: false);
        }
        catch (BadImageFormatException)
        {
            return null;
        }
        catch (FileLoadException)
        {
            return null;
        }
        catch (FileNotFoundException)
        {
            return null;
        }
        catch (NotSupportedException)
        {
            return null;
        }
        catch (ReflectionTypeLoadException)
        {
            return null;
        }
        catch (TypeLoadException)
        {
            return null;
        }
    }

    private static Type? GetTypeSafe(string assemblyQualifiedOrMetadataName)
    {
        try
        {
            return Type.GetType(assemblyQualifiedOrMetadataName, throwOnError: false);
        }
        catch (BadImageFormatException)
        {
            return null;
        }
        catch (FileLoadException)
        {
            return null;
        }
        catch (FileNotFoundException)
        {
            return null;
        }
        catch (NotSupportedException)
        {
            return null;
        }
        catch (ReflectionTypeLoadException)
        {
            return null;
        }
        catch (TypeLoadException)
        {
            return null;
        }
    }
}
