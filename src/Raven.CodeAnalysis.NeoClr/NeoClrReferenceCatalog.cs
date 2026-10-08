using System.Collections.Immutable;
using System.Text.RegularExpressions;

using NeoCLR.Metadata.Experimental;
using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>An explicit immutable set of primitive, native semantic and emission dependency snapshots.</summary>
/// <remarks>Hosts recreate the catalog when inputs change. It performs no dependency search, code loading,
/// caching, CLI projection or compilation. Semantic dependency errors remain compilation diagnostics.</remarks>
public sealed class NeoClrReferenceCatalog
{
    private readonly NativeLibraryDefinition? seed;
    private readonly NeoClrPrimitiveBootstrap? bootstrap;

    private NeoClrReferenceCatalog(NeoClrPrimitiveBootstrap bootstrap, ImmutableArray<MetadataReference> references,
        ImmutableArray<NeoClrMetadataDependency> dependencies, NativeLibraryDefinition? seed)
    {
        this.bootstrap = bootstrap;
        References = references;
        Dependencies = dependencies;
        this.seed = seed;
    }

    private NeoClrReferenceCatalog(NeoClrMetadataReference nativeCore, ImmutableArray<MetadataReference> references,
        ImmutableArray<NeoClrMetadataDependency> dependencies)
    {
        NativeCore = nativeCore;
        References = references;
        Dependencies = dependencies;
    }

    /// <summary>Gets the CLI primitive snapshot for catalogs created by Read.</summary>
    /// <exception cref="InvalidOperationException">The catalog was created by ReadNative and has no CLI bootstrap.</exception>
    public NeoClrPrimitiveBootstrap Bootstrap => bootstrap ?? throw new InvalidOperationException("Native catalog has no CLI bootstrap; use CoreReference.");
    /// <summary>Gets the native core snapshot for ReadNative catalogs, otherwise null.</summary>
    public NeoClrMetadataReference? NativeCore { get; }
    /// <summary>Gets the selected semantic core reference from References.</summary>
    public MetadataReference CoreReference => NativeCore is { } native ? native : Bootstrap.Reference;
    /// <summary>Gets whether this catalog requires native-only semantic loading.</summary>
    public bool UsesNativeMetadata => NativeCore is not null;
    /// <summary>Gets the exact core identity read from the selected snapshot.</summary>
    public AssemblyIdentity CoreIdentity => NativeCore?.Definition.Identity ?? Bootstrap.Definition.Identity;
    /// <summary>Gets compiler references, selected core first, followed by native inputs in host order.</summary>
    public ImmutableArray<MetadataReference> References { get; }
    /// <summary>Gets bindings using the same semantic reference instances, including the native core, or the explicit CLI runtime seed if supplied.</summary>
    public ImmutableArray<NeoClrMetadataDependency> Dependencies { get; }

    /// <summary>Reads each selected artifact once and constructs matching semantic and emission snapshots.</summary>
    /// <param name="corePath">Required ordinary CLI primitive bootstrap, at most 4 MiB.</param>
    /// <param name="nativeReferencePaths">Native PE/#Neo libraries, at most 16 MiB each; never projected to CLI.</param>
    /// <param name="runtimeSeedPath">Optional retained System runtime seed, at most 8 MiB. Adds no semantic declarations.</param>
    /// <returns>A new catalog; existing catalogs are unaffected by subsequent file changes.</returns>
    /// <exception cref="ArgumentException">Duplicate paths or identities, including the core identity.</exception>
    /// <exception cref="IOException">An artifact is missing, inaccessible, too large or changes length during reading.</exception>
    /// <exception cref="InvalidDataException">An artifact is malformed or has an unsupported metadata contract.</exception>
    public static NeoClrReferenceCatalog Read(string corePath, IEnumerable<string> nativeReferencePaths, string? runtimeSeedPath = null)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(corePath);
        ArgumentNullException.ThrowIfNull(nativeReferencePaths);
        var paths = nativeReferencePaths.Select(Path.GetFullPath).ToArray();
        var selected = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { Path.GetFullPath(corePath) };
        foreach (var path in paths)
            if (!selected.Add(path)) throw new ArgumentException("Duplicate input path: " + path);
        if (runtimeSeedPath is not null && !selected.Add(Path.GetFullPath(runtimeSeedPath)))
            throw new ArgumentException("Runtime seed must be distinct from metadata inputs.");
        var bootstrap = NeoClrPrimitiveBootstrap.ReadAssembly(ReadImage(corePath, 4 * 1024 * 1024));
        var references = ImmutableArray.CreateBuilder<MetadataReference>();
        var dependencies = ImmutableArray.CreateBuilder<NeoClrMetadataDependency>();
        references.Add(bootstrap.Reference);
        NativeLibraryDefinition? seed = null;
        if (runtimeSeedPath is not null)
        {
            seed = NativeLibraryDefinition.ReadAssembly(ReadImage(runtimeSeedPath, 8 * 1024 * 1024));
            if (seed.ModuleName != "System") throw new InvalidDataException("Runtime seed must declare module System.");
            dependencies.Add(new(bootstrap.Reference, bootstrap.Definition, bootstrap.Definition.Identity, seed));
        }
        var identities = new HashSet<AssemblyIdentity> { bootstrap.Definition.Identity };
        foreach (var path in paths)
        {
            var image = ReadImage(path, 16 * 1024 * 1024);
            _ = RuntimeAssemblyContainer.Read(image);
            var reference = NeoClrMetadataReference.ReadDocumentedAssembly(image, bootstrap, path);
            if (!identities.Add(reference.Definition.Identity))
                throw new ArgumentException("Duplicate native assembly identity: " + reference.Definition.Identity.Name);
            references.Add(reference);
            dependencies.Add(new(reference, bootstrap.Definition.Identity));
        }
        return new(bootstrap, references.ToImmutable(), dependencies.ToImmutable(), seed);
    }

    /// <summary>Snapshots a native semantic core and native libraries without a CLI bootstrap or projection.</summary>
    /// <param name="corePath">Native PE/#Neo core, at most 16 MiB; no core completeness is inferred from its filename.</param>
    /// <param name="nativeReferencePaths">Explicit native library paths, at most 16 MiB each, in host order.</param>
    /// <returns>An immutable catalog with the core first in References and Dependencies, using the same reference instances.</returns>
    /// <remarks>Use MetadataImportOptions.WithNativeMetadata and explicit runtime contracts. Runtime seeds are separate
    /// execution inputs, not translated implementations attached to the native core. Missing semantic dependencies remain
    /// compilation diagnostics. Adjacent XML documentation is optional, as for Read; no adjacent assemblies are searched.</remarks>
    /// <exception cref="ArgumentException">Paths or assembly identities are duplicated.</exception>
    /// <exception cref="IOException">An input cannot be read or changes length during reading.</exception>
    /// <exception cref="InvalidDataException">An input is oversized, malformed, or lacks the supported native metadata contract.</exception>
    public static NeoClrReferenceCatalog ReadNative(string corePath, IEnumerable<string> nativeReferencePaths)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(corePath);
        ArgumentNullException.ThrowIfNull(nativeReferencePaths);
        var paths = new[] { Path.GetFullPath(corePath) }.Concat(nativeReferencePaths.Select(Path.GetFullPath)).ToArray();
        if (paths.Distinct(StringComparer.OrdinalIgnoreCase).Count() != paths.Length)
            throw new ArgumentException("Duplicate native input path.", nameof(nativeReferencePaths));
        var references = ImmutableArray.CreateBuilder<MetadataReference>();
        var dependencies = ImmutableArray.CreateBuilder<NeoClrMetadataDependency>();
        var identities = new HashSet<AssemblyIdentity>();
        NeoClrMetadataReference? core = null;
        foreach (var path in paths)
        {
            var image = ReadImage(path, 16 * 1024 * 1024);
            _ = RuntimeAssemblyContainer.Read(image);
            var reference = NeoClrMetadataReference.ReadDocumentedAssembly(image, null, path);
            if (!identities.Add(reference.Definition.Identity))
                throw new ArgumentException("Duplicate native assembly identity: " + reference.Definition.Identity.Name);
            core ??= reference;
            references.Add(reference);
            dependencies.Add(new(reference, core.Definition.Identity));
        }
        return new(core!, references.ToImmutable(), dependencies.ToImmutable());
    }

    /// <summary>Rejects retained-seed declarations owned by selected source libraries.</summary>
    /// <param name="metadataTypeNames">Fully qualified metadata names, including generic arity and nested-type separators.</param>
    /// <exception cref="InvalidDataException">The retained seed contains a competing declaration.</exception>
    public void ValidateSourceOwnership(IEnumerable<string> metadataTypeNames)
    {
        ArgumentNullException.ThrowIfNull(metadataTypeNames);
        if (seed is null) return;
        foreach (var name in metadataTypeNames)
        {
            var nativeName = Regex.Replace(name.Replace('+', '.'), @"`\d+", "");
            if (seed.TypeNames.Contains(nativeName))
                throw new InvalidDataException("Runtime seed duplicates a source-owned declaration: " + name);
        }
    }

    private static byte[] ReadImage(string path, int limit)
    {
        using var stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read);
        if (stream.Length > limit) throw new InvalidDataException("Metadata input exceeds its image limit: " + path);
        var image = new byte[checked((int)stream.Length)];
        stream.ReadExactly(image);
        if (stream.ReadByte() != -1) throw new IOException("Metadata input changed length while reading: " + path);
        return image;
    }
}
