using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>Loads explicit native project artifacts using the same catalog as rvnc neoclr.</summary>
/// <remarks>Select RavenTargetPlatform=NeoCLR and RavenMetadataFormat=NeoCLR. This adapter supplies
/// semantic references, not build/run orchestration or automatic reference refresh.</remarks>
public sealed class NeoClrProjectMetadataProvider : IProjectMetadataProvider
{
    private readonly System.Collections.Concurrent.ConcurrentDictionary<string, NeoClrProjectConfiguration> configurations = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>Gets the last successfully loaded immutable host configuration for a project.</summary>
    public NeoClrProjectConfiguration GetConfiguration(string projectFilePath) => configurations.TryGetValue(Path.GetFullPath(projectFilePath), out var configuration)
        ? configuration : throw new InvalidOperationException("Project must successfully load RavenMetadataFormat=NeoCLR first.");

    /// <inheritdoc />
    public IReadOnlyList<string> GetInputPaths(string projectFilePath, IReadOnlyDictionary<string, string> properties)
        => new[] { "RavenNeoClrCoreReference", "RavenNeoClrRuntimeSeed", "RavenNeoClrBootstrapOwnership" }
            .Where(name => properties.TryGetValue(name, out var value) && !string.IsNullOrWhiteSpace(value))
            .Select(name => Path.GetFullPath(properties[name], Path.GetDirectoryName(Path.GetFullPath(projectFilePath))!)).ToArray();

    /// <inheritdoc />
    public string MetadataFormat => "NeoCLR";

    /// <inheritdoc />
    public string GetOutputPath(string projectFilePath, string assemblyName)
        => Path.Combine(Path.GetDirectoryName(Path.GetFullPath(projectFilePath))!, "bin", "neoclr", assemblyName + ".dll");

    /// <inheritdoc />
    public void ValidateProjectArtifact(string path, string assemblyName)
    {
        var definition = AssemblyDefinition.ReadNativeAssembly(File.ReadAllBytes(path));
        if (definition.Name != assemblyName)
            throw new InvalidDataException("Native project artifact identity does not match " + assemblyName + ": " + path);
    }

    /// <inheritdoc />
    public ProjectMetadataConfiguration Load(string projectFilePath, string assemblyName, CompilationOptions options,
        IReadOnlyDictionary<string, string> properties, IReadOnlyList<string> referencePaths)
    {
        if (options.TargetPlatform != TargetPlatform.NeoCLR)
            throw new InvalidDataException("Native metadata requires RavenTargetPlatform=NeoCLR.");
        var directory = Path.GetDirectoryName(Path.GetFullPath(projectFilePath))!;
        string? PathProperty(string name) => properties.TryGetValue(name, out var value) && !string.IsNullOrWhiteSpace(value)
            ? Path.GetFullPath(value, directory) : null;
        var core = PathProperty("RavenNeoClrCoreReference")
            ?? throw new InvalidDataException("Native metadata requires RavenNeoClrCoreReference.");
        var catalog = NeoClrReferenceCatalog.Read(core, referencePaths, PathProperty("RavenNeoClrRuntimeSeed"));
        if (options.MetadataImportOptions is { } imports && imports.CoreAssemblyName != catalog.CoreIdentity.Name)
            throw new InvalidDataException("Native project core identity does not match RavenMetadataCoreAssemblyName.");
        if (properties.TryGetValue("RavenTargetCoreAssemblyName", out var targetCore) &&
            !string.IsNullOrWhiteSpace(targetCore) && targetCore != catalog.CoreIdentity.Name)
            throw new InvalidDataException("Native project core identity does not match RavenTargetCoreAssemblyName.");
        var ownershipPath = PathProperty("RavenNeoClrBootstrapOwnership");
        var ownership = ownershipPath is null ? null : BootstrapOwnershipManifest.Read(ownershipPath);
        catalog.ValidateSourceOwnership(ownership?.Libraries.SelectMany(library => library.Types) ?? []);
        if (ownership is not null) options = ownership.Apply(options, assemblyName, catalog.CoreIdentity.Name);
        if (properties.TryGetValue("RavenNeoClrAsyncLibrary", out var asyncLibrary) && !string.IsNullOrWhiteSpace(asyncLibrary))
            options = options.WithMetadataImportOptions((options.MetadataImportOptions ?? new MetadataImportOptions(catalog.CoreIdentity.Name)).WithAsyncAssemblyName(asyncLibrary));
        string? objectRootPath = null;
        if (properties.TryGetValue("RavenNeoClrObjectLibrary", out var objectLibrary) && !string.IsNullOrWhiteSpace(objectLibrary))
        {
            var nativeReferences = catalog.References.OfType<NeoClrMetadataReference>().ToArray();
            var selected = nativeReferences.Select((reference, index) => (reference, index))
                .Where(item => item.reference.Definition.Name == objectLibrary).ToArray();
            if (selected.Length != 1)
                throw new InvalidDataException("Object library must select exactly one registered native reference: " + objectLibrary);
            objectRootPath = Path.GetFullPath(referencePaths[selected[0].index]);
            options = options.WithMetadataImportOptions((options.MetadataImportOptions ?? new MetadataImportOptions(catalog.CoreIdentity.Name))
                .WithObjectAssemblyName(objectLibrary));
        }
        var intrinsicText = properties.GetValueOrDefault("RavenNeoClrBootstrapIntrinsics");
        var intrinsic = false;
        if (!string.IsNullOrWhiteSpace(intrinsicText) && !bool.TryParse(intrinsicText, out intrinsic))
            throw new InvalidDataException("RavenNeoClrBootstrapIntrinsics must be true or false.");
        configurations[Path.GetFullPath(projectFilePath)] = new(catalog, referencePaths.ToArray(), PathProperty("RavenNeoClrRuntimeSeed"), ownership, intrinsic, objectRootPath);
        return new(options.WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithMetadataImportOptions(options.MetadataImportOptions ?? new MetadataImportOptions(catalog.CoreIdentity.Name)), catalog.References);
    }
}

/// <summary>Explicit host artifact snapshot shared by project import and native emission.</summary>
public sealed class NeoClrProjectConfiguration
{
    internal NeoClrProjectConfiguration(NeoClrReferenceCatalog catalog, string[] paths, string? seed, BootstrapOwnershipManifest? ownership, bool bootstrapIntrinsics, string? objectRootPath)
    { this.bootstrapIntrinsics = bootstrapIntrinsics; ObjectRootPath = objectRootPath; Catalog = catalog; ReferencePaths = Array.AsReadOnly(paths); RuntimeSeedPath = seed; this.ownership = ownership; }
    private readonly BootstrapOwnershipManifest? ownership;
    private readonly bool bootstrapIntrinsics;
    /// <summary>Semantic and emission artifact identities from one read.</summary>
    public NeoClrReferenceCatalog Catalog { get; }
    /// <summary>Explicit native runtime dependency paths.</summary>
    public IReadOnlyList<string> ReferencePaths { get; }
    /// <summary>Exact registered native artifact selected as the runtime Object root, if configured.</summary>
    public string? ObjectRootPath { get; }
    /// <summary>Optional retained runtime System seed.</summary>
    public string? RuntimeSeedPath { get; }
    /// <summary>Creates the native adapter from explicit artifacts, never from importer symbols.</summary>
    public NeoClrEmissionBackend CreateEmissionBackend(string assemblyName) => new(new(
        new(assemblyName, new Version(1, 0, 0, 0)), Catalog.CoreIdentity, Catalog.Dependencies,
        Catalog.Bootstrap.Reference, bootstrapReference: bootstrapIntrinsics ? Catalog.Bootstrap.Reference : null, primitiveImplementations: ownership?.NativePrimitives?
            .Where(p => p.Value == assemblyName && p.Key != "System.Char").Select(p => Enum.Parse<PrimitiveType>(p.Key[7..])),
        implementsGrapheme: ownership?.NativePrimitives?.GetValueOrDefault("System.Char") == assemblyName));

    /// <summary>Checks source/imported declaration ownership before publishing an assembly.</summary>
    public void Validate(Compilation compilation) => ownership?.Validate(compilation);
}
