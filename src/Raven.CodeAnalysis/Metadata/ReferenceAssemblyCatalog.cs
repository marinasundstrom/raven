using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Runtime.Loader;

namespace Raven.CodeAnalysis.Metadata;

/// <summary>
/// Represents the set of reference assemblies provided to a <see cref="Compilation"/>.
/// The catalog parses metadata with <see cref="System.Reflection.Metadata"/> so Raven can
/// reason about reference-only assemblies without relying on <see cref="System.Reflection.MetadataLoadContext"/>.
/// It also offers a transitional bridge to load runtime assemblies when the legacy
/// Reflection.Emit pipeline still requires <see cref="System.Type"/> handles.
/// </summary>
internal sealed class ReferenceAssemblyCatalog
{
    private readonly ImmutableDictionary<PortableExecutableReference, ReferenceEntry> _referenceMap;
    private readonly ImmutableDictionary<string, ReferenceEntry> _identityMap;
    private static readonly Lazy<ImmutableDictionary<string, string>> s_trustedPlatformAssemblies
        = new(BuildTrustedPlatformAssemblyMap);

    private readonly ConcurrentDictionary<string, Lazy<Assembly?>> _runtimeAssemblies = new(StringComparer.Ordinal);
    private readonly ConcurrentDictionary<string, Lazy<Assembly?>> _implementationAssemblies = new(StringComparer.Ordinal);

    public ReferenceAssemblyCatalog(IEnumerable<MetadataReference> references)
    {
        if (references is null)
            throw new ArgumentNullException(nameof(references));

        var referenceBuilder = ImmutableDictionary
            .CreateBuilder<PortableExecutableReference, ReferenceEntry>(ReferenceEqualityComparer.Instance);

        var identityBuilder = ImmutableDictionary.CreateBuilder<string, ReferenceEntry>(StringComparer.Ordinal);

        foreach (var reference in references)
        {
            if (reference is not PortableExecutableReference peReference)
                continue;

            if (string.IsNullOrEmpty(peReference.FilePath) || !File.Exists(peReference.FilePath))
                continue;

            var entry = CreateEntry(peReference);

            referenceBuilder[peReference] = entry;

            if (entry.Identity.FullName is { Length: > 0 } fullName && !identityBuilder.ContainsKey(fullName))
            {
                identityBuilder[fullName] = entry;
            }
        }

        _referenceMap = referenceBuilder.ToImmutable();
        _identityMap = identityBuilder.ToImmutable();

        CoreAssembly = typeof(object).Assembly;
    }

    /// <summary>
    /// The runtime core library used as a stand-in for <c>mscorlib</c>/System.Private.CoreLib.
    /// </summary>
    public Assembly CoreAssembly { get; }

    /// <summary>
    /// Attempts to materialize a runtime <see cref="Assembly"/> corresponding to the provided
    /// metadata reference.
    /// </summary>
    public Assembly GetRuntimeAssembly(PortableExecutableReference reference)
    {
        if (reference is null)
            throw new ArgumentNullException(nameof(reference));

        if (!_referenceMap.TryGetValue(reference, out var entry))
            entry = CreateEntry(reference);

        return LoadRuntimeAssembly(entry, allowReferenceFallback: true)
            ?? throw new InvalidOperationException($"Unable to load runtime assembly for reference '{reference.FilePath}'.");
    }

    /// <summary>
    /// Tries to resolve a runtime assembly for the given identity. Returns <c>null</c>
    /// when the assembly cannot be found on the machine.
    /// </summary>
    public Assembly? TryGetRuntimeAssembly(AssemblyName identity)
    {
        if (identity is null)
            throw new ArgumentNullException(nameof(identity));

        var cacheKey = identity.FullName ?? identity.Name;
        if (string.IsNullOrEmpty(cacheKey))
            return null;

        if (_identityMap.TryGetValue(cacheKey, out var entry))
            return LoadRuntimeAssembly(entry, allowReferenceFallback: true);

        return LoadByIdentity(cacheKey, identity, allowReferenceFallback: true);
    }

    public Assembly? TryGetImplementationAssembly(PortableExecutableReference reference)
    {
        if (reference is null)
            throw new ArgumentNullException(nameof(reference));

        if (!_referenceMap.TryGetValue(reference, out var entry))
            entry = CreateEntry(reference);

        return LoadRuntimeAssembly(entry, allowReferenceFallback: false);
    }

    public Assembly? TryGetImplementationAssembly(AssemblyName identity)
    {
        if (identity is null)
            throw new ArgumentNullException(nameof(identity));

        var cacheKey = identity.FullName ?? identity.Name;
        if (string.IsNullOrEmpty(cacheKey))
            return null;

        if (_identityMap.TryGetValue(cacheKey, out var entry))
            return LoadRuntimeAssembly(entry, allowReferenceFallback: false);

        return LoadByIdentity(cacheKey, identity, allowReferenceFallback: false);
    }

    private Assembly? LoadRuntimeAssembly(ReferenceEntry entry, bool allowReferenceFallback)
    {
        var cacheKey = entry.Identity.FullName ?? entry.Identity.Name;
        var cache = allowReferenceFallback ? _runtimeAssemblies : _implementationAssemblies;

        if (string.IsNullOrEmpty(cacheKey))
            return LoadFromEntry(entry, allowReferenceFallback);

        var lazy = cache.GetOrAdd(cacheKey, _ => new Lazy<Assembly?>(() => LoadFromEntry(entry, allowReferenceFallback)));
        return lazy.Value;
    }

    private static ReferenceEntry CreateEntry(PortableExecutableReference reference)
    {
        if (reference is null)
            throw new ArgumentNullException(nameof(reference));

        if (string.IsNullOrEmpty(reference.FilePath))
            throw new InvalidOperationException("Portable executable reference must have a file path.");

        using var stream = File.OpenRead(reference.FilePath);
        using var peReader = new PEReader(stream);
        var metadataReader = peReader.GetMetadataReader();

        if (!metadataReader.IsAssembly)
            throw new InvalidOperationException($"Reference '{reference.FilePath}' does not describe an assembly.");

        var definition = metadataReader.GetAssemblyDefinition();
        var name = metadataReader.GetString(definition.Name);
        var culture = metadataReader.GetString(definition.Culture);

        var assemblyName = new AssemblyName(name)
        {
            Version = definition.Version,
            CultureName = string.IsNullOrEmpty(culture) ? null : culture,
            ContentType = ConvertContentType(definition.Flags)
        };

        if (!definition.PublicKey.IsNil)
        {
            assemblyName.SetPublicKey(metadataReader.GetBlobBytes(definition.PublicKey));
            assemblyName.Flags |= AssemblyNameFlags.PublicKey;
        }

        return new ReferenceEntry(reference, reference.FilePath!, assemblyName);
    }

    private Assembly? LoadFromEntry(ReferenceEntry entry, bool allowReferenceFallback)
    {
        // First attempt to bind using the assembly identity. This will pick up runtime
        // implementation assemblies when they are available on the machine.
        var cacheKey = entry.Identity.FullName ?? entry.Identity.Name;
        if (!string.IsNullOrEmpty(cacheKey))
        {
            var candidate = LoadByIdentity(cacheKey, entry.Identity, allowReferenceFallback);
            if (candidate is not null)
                return candidate;
        }

        if (!allowReferenceFallback)
            return null;

        // Fall back to loading the metadata file directly. Reference assemblies contain metadata
        // only but the loader is capable of materializing them for inspection scenarios.
        try
        {
            return AssemblyLoadContext.Default.LoadFromAssemblyPath(entry.FilePath);
        }
        catch
        {
            return null;
        }
    }

    private Assembly? LoadByIdentity(string cacheKey, AssemblyName identity, bool allowReferenceFallback)
    {
        try
        {
            return Assembly.Load(identity);
        }
        catch
        {
            // ignored
        }

        if (identity.Name is { Length: > 0 } simpleName)
        {
            try
            {
                var relaxedIdentity = new AssemblyName(simpleName)
                {
                    ContentType = identity.ContentType,
                    CultureName = identity.CultureName
                };

                var publicKeyToken = identity.GetPublicKeyToken();
                if (publicKeyToken is { Length: > 0 })
                    relaxedIdentity.SetPublicKeyToken(publicKeyToken);

                return Assembly.Load(relaxedIdentity);
            }
            catch
            {
                // ignored
            }

            var trustedAssemblies = s_trustedPlatformAssemblies.Value;
            if (trustedAssemblies.TryGetValue(simpleName, out var trustedPath) && File.Exists(trustedPath))
            {
                try
                {
                    return AssemblyLoadContext.Default.LoadFromAssemblyPath(trustedPath);
                }
                catch
                {
                    // ignored
                }
            }
        }

        if (!allowReferenceFallback)
            return null;

        if (_identityMap.TryGetValue(cacheKey, out var entry) && File.Exists(entry.FilePath))
        {
            try
            {
                return AssemblyLoadContext.Default.LoadFromAssemblyPath(entry.FilePath);
            }
            catch
            {
                return null;
            }
        }

        return null;
    }

    private static AssemblyContentType ConvertContentType(AssemblyFlags flags)
    {
        return (flags & AssemblyFlags.WindowsRuntime) != 0
            ? AssemblyContentType.WindowsRuntime
            : AssemblyContentType.Default;
    }

    private static ImmutableDictionary<string, string> BuildTrustedPlatformAssemblyMap()
    {
        var data = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string;
        if (string.IsNullOrEmpty(data))
            return ImmutableDictionary<string, string>.Empty;

        var builder = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.OrdinalIgnoreCase);

        foreach (var entry in data.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries))
        {
            if (string.IsNullOrEmpty(entry))
                continue;

            var simpleName = Path.GetFileNameWithoutExtension(entry);
            if (string.IsNullOrEmpty(simpleName) || builder.ContainsKey(simpleName))
                continue;

            builder[simpleName] = entry;
        }

        return builder.ToImmutable();
    }

    private readonly record struct ReferenceEntry(
        PortableExecutableReference Reference,
        string FilePath,
        AssemblyName Identity);
}
