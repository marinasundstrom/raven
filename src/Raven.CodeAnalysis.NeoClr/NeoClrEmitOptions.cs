using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

/// <summary>A host-supplied binding between a compiler reference and its read-only metadata contract.</summary>
/// <remarks>The host must keep the reference and snapshot consistent and provide matching native dependency artifacts.
/// A NeoClrMetadataReference requires its exact Definition snapshot and no translated nativeImplementation binding.</remarks>
public sealed class NeoClrMetadataDependency
{
    /// <summary>Creates a binding; no files or runtime assemblies are loaded.</summary>
    /// <param name="reference">The exact reference registered with the compilation.</param>
    /// <param name="definition">Snapshot describing that reference.</param>
    /// <param name="coreLibrary">Explicit dependency core contract.</param>
    /// <param name="nativeImplementation">Optional translated native inventory; selected imports must match its physical names and signatures.</param>
    public NeoClrMetadataDependency(MetadataReference reference, AssemblyDefinition definition, AssemblyIdentity coreLibrary, NativeLibraryDefinition? nativeImplementation = null)
    {
        ArgumentNullException.ThrowIfNull(reference);
        ArgumentNullException.ThrowIfNull(definition);
        ArgumentNullException.ThrowIfNull(coreLibrary);
        Reference = reference;
        Definition = definition;
        CoreLibrary = coreLibrary;
        NativeImplementation = nativeImplementation;
    }
    /// <summary>Gets an explicitly bound translated native implementation, or null for the metadata writer naming contract.</summary>
    public NativeLibraryDefinition? NativeImplementation { get; }
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
    public NeoClrEmitOptions(AssemblyIdentity identity, AssemblyIdentity coreLibrary, IEnumerable<NeoClrMetadataDependency> dependencies, MetadataReference? consoleReference = null, NeoClrSystemSymbols? systemSymbols = null, MetadataReference? bootstrapReference = null)
    {
        ArgumentNullException.ThrowIfNull(identity);
        ArgumentNullException.ThrowIfNull(coreLibrary);
        ArgumentNullException.ThrowIfNull(dependencies);
        Identity = identity;
        CoreLibrary = coreLibrary;
        ConsoleReference = consoleReference;
        SystemSymbols = systemSymbols;
        BootstrapReference = bootstrapReference;
        Dependencies = dependencies.ToImmutableArray();
        if (Dependencies.Any(d => d is null)) throw new ArgumentException("Null dependency", nameof(dependencies));
    }
    /// <summary>Gets the explicit implementation seed authorizing CheckedStorage.Reserve&lt;T&gt;(Int32).</summary>
    /// <remarks>Null disables native bootstrap intrinsics. The exact registered reference must supply the selected core; ordinary .NET emission is unchanged.</remarks>
    public MetadataReference? BootstrapReference { get; }
    /// <summary>Gets the unsigned output identity; its name must match the compilation.</summary>
    public AssemblyIdentity Identity { get; }
    /// <summary>Gets the explicit core-library identity for primitive contracts.</summary>
    public AssemblyIdentity CoreLibrary { get; }
    /// <summary>Gets the explicit compiler reference authorizing System.Console.WriteLine(string literal) mapping; null disables it.</summary>
    /// <remarks>The exact instance must be registered in the compilation. No other console overload or operation is mapped.</remarks>
    public MetadataReference? ConsoleReference { get; }
    /// <summary>Gets an explicit partial native System callable binding, or null.</summary>
    public NeoClrSystemSymbols? SystemSymbols { get; }
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


/// <summary>A host-bound, explicitly partial static-callable view of translated native System metadata.</summary>
/// <remarks>The host must build Reference from Library.CreateStaticInt32ReferenceAssembly with these exact selections.
/// Only the selected methods are bound; this is not a complete runtime core library or a native symbol provider.</remarks>
public sealed class NeoClrSystemSymbols
{
    /// <summary>Copies an explicit selection and associates its projected compiler reference.</summary>
    public NeoClrSystemSymbols(MetadataReference reference, string projectionAssemblyName, NativeLibraryDefinition library,
        IEnumerable<NativeFunctionDefinition> functions)
    {
        ArgumentNullException.ThrowIfNull(reference);
        ArgumentNullException.ThrowIfNull(projectionAssemblyName);
        ArgumentNullException.ThrowIfNull(library);
        ArgumentNullException.ThrowIfNull(functions);
        Reference = reference;
        ProjectionAssemblyName = projectionAssemblyName;
        Library = library;
        Functions = functions.ToImmutableArray();
    }
    /// <summary>Gets the exact compiler reference instance for the static callable projection.</summary>
    public MetadataReference Reference { get; }
    /// <summary>Gets the explicit synthetic projection identity's simple name.</summary>
    public string ProjectionAssemblyName { get; }
    /// <summary>Gets the native inventory owning every selected method.</summary>
    public NativeLibraryDefinition Library { get; }
    /// <summary>Gets the immutable explicit function selection.</summary>
    public ImmutableArray<NativeFunctionDefinition> Functions { get; }
}
