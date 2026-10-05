namespace Raven.CodeAnalysis.NeoClr;

/// <summary>Loads explicit native project artifacts using the same catalog as rvnc neoclr.</summary>
/// <remarks>Select RavenTargetPlatform=NeoCLR and RavenMetadataFormat=NeoCLR. This adapter supplies
/// semantic references, not build/run orchestration or automatic reference refresh.</remarks>
public sealed class NeoClrProjectMetadataProvider : IProjectMetadataProvider
{
    /// <inheritdoc />
    public string MetadataFormat => "NeoCLR";

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
        return new(options.WithTargetCoreAssemblyName(catalog.CoreIdentity.Name)
            .WithMetadataImportOptions(options.MetadataImportOptions ?? new MetadataImportOptions(catalog.CoreIdentity.Name)), catalog.References);
    }
}
