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

    private NeoClrReferenceCatalog(NeoClrPrimitiveBootstrap bootstrap, ImmutableArray<MetadataReference> references,
        ImmutableArray<NeoClrMetadataDependency> dependencies, NativeLibraryDefinition? seed)
    {
        Bootstrap = bootstrap;
        References = references;
        Dependencies = dependencies;
        this.seed = seed;
    }

    /// <summary>Gets the single primitive snapshot shared by all native references.</summary>
    public NeoClrPrimitiveBootstrap Bootstrap { get; }
    /// <summary>Gets the exact core identity read from that primitive snapshot.</summary>
    public AssemblyIdentity CoreIdentity => Bootstrap.Definition.Identity;
    /// <summary>Gets compiler references, primitive bootstrap first, followed by native inputs in host order.</summary>
    public ImmutableArray<MetadataReference> References { get; }
    /// <summary>Gets bindings using the same semantic reference instances, including the explicit runtime seed if supplied.</summary>
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
