using System.Reflection;

namespace Raven.CodeAnalysis;

/// <summary>
/// Specifies options that affect the emitted assembly artifact.
/// </summary>
public sealed class EmitOptions
{
    private readonly string? _targetCoreLibraryIdentity;

    /// <summary>
    /// Initializes emit options with an optional target core-library identity.
    /// </summary>
    /// <param name="targetCoreLibraryIdentity">
    /// The core-library identity that emitted host core type references should
    /// target, or <see langword="null"/> to use the normal .NET emission policy.
    /// </param>
    public EmitOptions(AssemblyName? targetCoreLibraryIdentity = null)
        : this(targetCoreLibraryIdentity, null)
    {
    }

    private EmitOptions(AssemblyName? targetCoreLibraryIdentity, ICompilationEmissionBackend? backend)
    {
        _targetCoreLibraryIdentity = targetCoreLibraryIdentity?.FullName;
        Backend = backend;
    }

    /// <summary>
    /// Gets the target core-library identity, or <see langword="null"/> when
    /// Raven should use its normal .NET emission policy.
    /// </summary>
    public AssemblyName? TargetCoreLibraryIdentity => _targetCoreLibraryIdentity is null
        ? null
        : new AssemblyName(_targetCoreLibraryIdentity);

    /// <summary>Gets the explicit artifact backend, or null for the compilation target's default emitter.</summary>
    public ICompilationEmissionBackend? Backend { get; }

    /// <summary>Creates options selecting an artifact backend without changing semantic target contracts.</summary>
    /// <param name="backend">Immutable, reusable backend configuration, or null to restore default emission.</param>
    public EmitOptions WithBackend(ICompilationEmissionBackend? backend)
        => new(TargetCoreLibraryIdentity, backend);

    /// <summary>
    /// Creates options with the specified target core-library identity.
    /// </summary>
    public EmitOptions WithTargetCoreLibraryIdentity(AssemblyName? targetCoreLibraryIdentity)
        => new(targetCoreLibraryIdentity, Backend);
}
