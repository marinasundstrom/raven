namespace Raven.CodeAnalysis;

/// <summary>
/// Imports metadata exclusively from the compilation's supplied PE references.
/// The caller must supply the core assembly and all required transitive dependencies.
/// This controls metadata binding, not the compiler host or emitted assembly retargeting.
/// </summary>
public sealed record MetadataImportOptions
{
    public MetadataImportOptions(string coreAssemblyName)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(coreAssemblyName);
        CoreAssemblyName = coreAssemblyName;
    }

    public string CoreAssemblyName { get; }
}
