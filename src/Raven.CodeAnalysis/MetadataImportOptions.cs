using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>
/// Imports metadata exclusively from the compilation's supplied references.
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

    public MetadataImportOptions(string coreAssemblyName) : this(coreAssemblyName, null)
    {
    }

    /// <summary>Selects a bootstrap core and explicit native numeric, Boolean, String, grapheme Char or runtime-handle declaration providers.</summary>
    public MetadataImportOptions(string coreAssemblyName, IReadOnlyDictionary<SpecialType, string>? primitiveAssemblies)
    : this(coreAssemblyName, primitiveAssemblies, null)
    {
    }

    /// <summary>Selects imported primitive providers and explicit source member providers while retaining bootstrap scalar identities.</summary>
    /// <param name="coreAssemblyName">The explicit primitive bootstrap assembly.</param>
    /// <param name="primitiveAssemblies">Native declarations imported from other assemblies.</param>
    /// <param name="sourcePrimitiveTypes">Primitives whose members are declared in this compilation. Cannot overlap imported providers.</param>
    /// <exception cref="ArgumentException">A provider is unsupported, conflicting or has an empty identity.</exception>
    public MetadataImportOptions(string coreAssemblyName, IReadOnlyDictionary<SpecialType, string>? primitiveAssemblies, IEnumerable<SpecialType>? sourcePrimitiveTypes)
        : this(coreAssemblyName, primitiveAssemblies, sourcePrimitiveTypes, false)
    {
    }

    /// <summary>Selects source System.Object as the semantic root for a NeoCLR bootstrap compilation.</summary>
    /// <param name="coreAssemblyName">Explicit bootstrap core for the remaining platform declarations.</param>
    /// <param name="primitiveAssemblies">Imported primitive member providers.</param>
    /// <param name="sourcePrimitiveTypes">Source primitive member providers.</param>
    /// <param name="useSourceObjectRoot">Select the compilation's own public abstract fieldless System.Object. Missing roots never fall back.</param>
    /// <exception cref="ArgumentException">The core identity is empty or a primitive provider is unsupported, conflicting or empty.</exception>
    /// <remarks>Native emission validates supported root declarations and bodies. The .NET emitter rejects this configuration before publication.</remarks>
    public MetadataImportOptions(string coreAssemblyName, IReadOnlyDictionary<SpecialType, string>? primitiveAssemblies, IEnumerable<SpecialType>? sourcePrimitiveTypes, bool useSourceObjectRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(coreAssemblyName);
        CoreAssemblyName = coreAssemblyName;
        UseSourceObjectRoot = useSourceObjectRoot;
        PrimitiveAssemblies = primitiveAssemblies?.ToImmutableDictionary() ?? ImmutableDictionary<SpecialType, string>.Empty;
        if (PrimitiveAssemblies.Any(p => !SupportsPrimitive(p.Key) || string.IsNullOrWhiteSpace(p.Value)))
            throw new ArgumentException("primitive providers require numeric, Boolean, String, grapheme Char or runtime-handle special types and assembly names", nameof(primitiveAssemblies));
        SourcePrimitiveTypes = sourcePrimitiveTypes?.ToImmutableHashSet() ?? ImmutableHashSet<SpecialType>.Empty;
        if (SourcePrimitiveTypes.Any(p => !SupportsPrimitive(p) || PrimitiveAssemblies.ContainsKey(p)))
            throw new ArgumentException("source primitive providers must be supported and distinct from imported providers", nameof(sourcePrimitiveTypes));
    }

    /// <summary>Gets whether this NeoCLR compilation explicitly owns the semantic System.Object root.</summary>
    /// <remarks>Does not change the bootstrap core. Native emission separately validates supported root declarations and bodies; CLI root emission remains unsupported.</remarks>
    public bool UseSourceObjectRoot { get; }

    /// <summary>Gets the explicitly selected native library owning Task and async builder declarations.</summary>
    /// <remarks>NeoCLR only. Missing or incompatible declarations never fall back to the CLI bootstrap.</remarks>
    public string? AsyncAssemblyName { get; private init; }

    /// <summary>Selects a native async declaration library, or clears the selection with null.</summary>
    /// <param name="assemblyName">Registered native assembly name; artifact identity is validated by the native reference catalog.</param>
    /// <returns>A new immutable import configuration.</returns>
    /// <exception cref="ArgumentException">The assembly name is empty or whitespace.</exception>
    public MetadataImportOptions WithAsyncAssemblyName(string? assemblyName)
    {
        if (assemblyName is not null) ArgumentException.ThrowIfNullOrWhiteSpace(assemblyName);
        return this with { AsyncAssemblyName = assemblyName };
    }

    private static bool SupportsPrimitive(SpecialType type) => type is SpecialType.System_SByte or SpecialType.System_Byte or
        SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_UInt32 or
        SpecialType.System_Int64 or SpecialType.System_UInt64 or SpecialType.System_Single or SpecialType.System_Double or
        SpecialType.System_Boolean or SpecialType.System_String or SpecialType.System_Char or SpecialType.System_RuntimeTypeHandle;

    /// <summary>
    /// Gets the explicit core identity, or null to discover it from supplied references.
    /// </summary>
    public string? CoreAssemblyName { get; }

    /// <summary>Gets explicit native numeric, Boolean, String, grapheme Char or runtime-handle declaration providers. Missing providers never fall back to the CLI bootstrap.</summary>
    /// <remarks>Supported only by the NeoCLR target. The primitive core remains required for other bootstrap declarations.</remarks>
    public ImmutableDictionary<SpecialType, string> PrimitiveAssemblies { get; } = ImmutableDictionary<SpecialType, string>.Empty;
    /// <summary>Gets explicitly source-owned primitive member declarations. Scalar signatures still use the selected bootstrap.</summary>
    /// <remarks>NeoCLR only. Member lookup uses the source declaration, without adding members to metadata imports or changing ordinary .NET lookup.</remarks>
    public ImmutableHashSet<SpecialType> SourcePrimitiveTypes { get; } = ImmutableHashSet<SpecialType>.Empty;
}
