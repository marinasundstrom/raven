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

    /// <summary>Selects a bootstrap core and explicit native numeric, String or grapheme Char declaration providers.</summary>
    public MetadataImportOptions(string coreAssemblyName, IReadOnlyDictionary<SpecialType, string>? primitiveAssemblies)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(coreAssemblyName);
        CoreAssemblyName = coreAssemblyName;
        PrimitiveAssemblies = primitiveAssemblies?.ToImmutableDictionary() ?? ImmutableDictionary<SpecialType, string>.Empty;
        if (PrimitiveAssemblies.Any(p => p.Key is not (SpecialType.System_SByte or SpecialType.System_Byte or
            SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_UInt32 or
            SpecialType.System_Int64 or SpecialType.System_UInt64 or SpecialType.System_Single or SpecialType.System_Double or SpecialType.System_String or SpecialType.System_Char) || string.IsNullOrWhiteSpace(p.Value)))
            throw new ArgumentException("primitive providers require numeric, String or grapheme Char special types and assembly names", nameof(primitiveAssemblies));
    }

    /// <summary>
    /// Gets the explicit core identity, or null to discover it from supplied references.
    /// </summary>
    public string? CoreAssemblyName { get; }

    /// <summary>Gets explicit native numeric, String or grapheme Char declaration providers. Missing providers never fall back to the CLI bootstrap.</summary>
    /// <remarks>Supported only by the NeoCLR target. The primitive core remains required for other bootstrap declarations.</remarks>
    public ImmutableDictionary<SpecialType, string> PrimitiveAssemblies { get; } = ImmutableDictionary<SpecialType, string>.Empty;
}
