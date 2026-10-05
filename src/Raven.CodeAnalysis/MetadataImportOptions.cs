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

    /// <summary>Selects a bootstrap core and explicit native numeric, Boolean, String or grapheme Char declaration providers.</summary>
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
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(coreAssemblyName);
        CoreAssemblyName = coreAssemblyName;
        PrimitiveAssemblies = primitiveAssemblies?.ToImmutableDictionary() ?? ImmutableDictionary<SpecialType, string>.Empty;
        if (PrimitiveAssemblies.Any(p => !SupportsPrimitive(p.Key) || string.IsNullOrWhiteSpace(p.Value)))
            throw new ArgumentException("primitive providers require numeric, Boolean, String or grapheme Char special types and assembly names", nameof(primitiveAssemblies));
        SourcePrimitiveTypes = sourcePrimitiveTypes?.ToImmutableHashSet() ?? ImmutableHashSet<SpecialType>.Empty;
        if (SourcePrimitiveTypes.Any(p => !SupportsPrimitive(p) || PrimitiveAssemblies.ContainsKey(p)))
            throw new ArgumentException("source primitive providers must be supported and distinct from imported providers", nameof(sourcePrimitiveTypes));
    }

    private static bool SupportsPrimitive(SpecialType type) => type is SpecialType.System_SByte or SpecialType.System_Byte or
        SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_UInt32 or
        SpecialType.System_Int64 or SpecialType.System_UInt64 or SpecialType.System_Single or SpecialType.System_Double or
        SpecialType.System_Boolean or SpecialType.System_String or SpecialType.System_Char;

    /// <summary>
    /// Gets the explicit core identity, or null to discover it from supplied references.
    /// </summary>
    public string? CoreAssemblyName { get; }

    /// <summary>Gets explicit native numeric, Boolean, String or grapheme Char declaration providers. Missing providers never fall back to the CLI bootstrap.</summary>
    /// <remarks>Supported only by the NeoCLR target. The primitive core remains required for other bootstrap declarations.</remarks>
    public ImmutableDictionary<SpecialType, string> PrimitiveAssemblies { get; } = ImmutableDictionary<SpecialType, string>.Empty;
    /// <summary>Gets explicitly source-owned primitive member declarations. Scalar signatures still use the selected bootstrap.</summary>
    /// <remarks>NeoCLR only. Member lookup uses the source declaration, without adding members to metadata imports or changing ordinary .NET lookup.</remarks>
    public ImmutableHashSet<SpecialType> SourcePrimitiveTypes { get; } = ImmutableHashSet<SpecialType>.Empty;
}

