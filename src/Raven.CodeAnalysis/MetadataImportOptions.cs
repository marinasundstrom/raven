namespace Raven.CodeAnalysis;

/// <summary>
/// Imports metadata exclusively from the compilation's supplied PE references.
/// The caller must supply the core assembly and all required transitive dependencies.
/// A named core controls metadata binding independently of emission selection.
/// With a discovered core, the .NET target uses that identity for emission too.
/// Neither mode changes the compiler host.
/// </summary>
public sealed record MetadataImportOptions
{
    /// <summary>
    /// Uses only supplied references and discovers their core library. The .NET
    /// target uses the discovered core for both binding and emission.
    /// </summary>
    public MetadataImportOptions()
    {
    }

    public MetadataImportOptions(string coreAssemblyName)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(coreAssemblyName);
        CoreAssemblyName = coreAssemblyName;
    }

    /// <summary>
    /// Gets the explicit core identity, or null to discover it from supplied references.
    /// </summary>
    public string? CoreAssemblyName { get; }
}
