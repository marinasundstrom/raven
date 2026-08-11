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
    {
        _targetCoreLibraryIdentity = targetCoreLibraryIdentity?.FullName;
    }

    /// <summary>
    /// Gets the target core-library identity, or <see langword="null"/> when
    /// Raven should use its normal .NET emission policy.
    /// </summary>
    public AssemblyName? TargetCoreLibraryIdentity => _targetCoreLibraryIdentity is null
        ? null
        : new AssemblyName(_targetCoreLibraryIdentity);

    /// <summary>
    /// Creates options with the specified target core-library identity.
    /// </summary>
    public EmitOptions WithTargetCoreLibraryIdentity(AssemblyName? targetCoreLibraryIdentity)
        => new(targetCoreLibraryIdentity);
}
