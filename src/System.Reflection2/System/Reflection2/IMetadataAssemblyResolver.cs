namespace System.Reflection2;

using System.Diagnostics.CodeAnalysis;
using System.Reflection.Metadata;

/// <summary>
/// Resolves assembly metadata for <see cref="MetadataLoadContext"/>.
/// </summary>
public interface IMetadataAssemblyResolver
{
    /// <summary>
    /// Attempts to resolve an assembly by its display name.
    /// </summary>
    /// <param name="assemblyName">The simple or display name of the assembly.</param>
    /// <param name="result">When successful, contains the resolved metadata.</param>
    /// <returns><c>true</c> when the assembly could be resolved; otherwise, <c>false</c>.</returns>
    bool TryResolve(string assemblyName, [NotNullWhen(true)] out MetadataResolutionResult? result);

    /// <summary>
    /// Attempts to resolve an assembly from an <see cref="AssemblyReferenceHandle"/>.
    /// </summary>
    /// <param name="reader">The metadata reader that owns the reference.</param>
    /// <param name="handle">The assembly reference handle.</param>
    /// <param name="result">When successful, contains the resolved metadata.</param>
    /// <returns><c>true</c> when the assembly could be resolved; otherwise, <c>false</c>.</returns>
    bool TryResolve(MetadataReader reader, AssemblyReferenceHandle handle, [NotNullWhen(true)] out MetadataResolutionResult? result);
}
