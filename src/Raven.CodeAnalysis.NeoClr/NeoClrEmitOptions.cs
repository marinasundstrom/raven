using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>A host-supplied binding between a compiler reference and its read-only metadata contract.</summary>
/// <remarks>The host must keep the reference and snapshot consistent and provide matching native dependency artifacts.</remarks>
public sealed class NeoClrMetadataDependency
{
    /// <summary>Creates a binding; no files or runtime assemblies are loaded.</summary>
    /// <param name="reference">The exact reference registered with the compilation.</param>
    /// <param name="definition">Snapshot describing that reference.</param>
    /// <param name="coreLibrary">Explicit dependency core contract.</param>
    public NeoClrMetadataDependency(MetadataReference reference, AssemblyDefinition definition, AssemblyIdentity coreLibrary)
    {
        ArgumentNullException.ThrowIfNull(reference);
        ArgumentNullException.ThrowIfNull(definition);
        ArgumentNullException.ThrowIfNull(coreLibrary);
        Reference = reference;
        Definition = definition;
        CoreLibrary = coreLibrary;
    }
    /// <summary>Gets the compiler reference whose assembly symbol identifies calls.</summary>
    public MetadataReference Reference { get; }
    /// <summary>Gets the read-only dependency snapshot.</summary>
    public AssemblyDefinition Definition { get; }
    /// <summary>Gets the host-asserted core contract.</summary>
    public AssemblyIdentity CoreLibrary { get; }
}

/// <summary>Explicit output identity, primitive core contract and dependency bindings for the bounded native emitter.</summary>
public sealed class NeoClrEmitOptions
{
    /// <summary>Copies the supplied bindings into an immutable configuration.</summary>
    public NeoClrEmitOptions(AssemblyIdentity identity, AssemblyIdentity coreLibrary, IEnumerable<NeoClrMetadataDependency> dependencies)
    {
        ArgumentNullException.ThrowIfNull(identity);
        ArgumentNullException.ThrowIfNull(coreLibrary);
        ArgumentNullException.ThrowIfNull(dependencies);
        Identity = identity;
        CoreLibrary = coreLibrary;
        Dependencies = dependencies.ToImmutableArray();
        if (Dependencies.Any(d => d is null)) throw new ArgumentException("Null dependency", nameof(dependencies));
    }
    /// <summary>Gets the unsigned output identity; its name must match the compilation.</summary>
    public AssemblyIdentity Identity { get; }
    /// <summary>Gets the explicit core-library identity for Int32 contracts.</summary>
    public AssemblyIdentity CoreLibrary { get; }
    /// <summary>Gets immutable host bindings. Duplicate identities/symbols are rejected during emission.</summary>
    public ImmutableArray<NeoClrMetadataDependency> Dependencies { get; }
}

/// <summary>Emission outcome with Raven diagnostics; failed validation writes no output.</summary>
public sealed class NeoClrEmitResult
{
    internal NeoClrEmitResult(bool success, ImmutableArray<Diagnostic> diagnostics) { Success = success; Diagnostics = diagnostics; }
    /// <summary>Gets whether validated native bytes were written.</summary>
    public bool Success { get; }
    /// <summary>Gets preserved compiler diagnostics and any native backend diagnostic.</summary>
    public ImmutableArray<Diagnostic> Diagnostics { get; }
}
