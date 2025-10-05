namespace Raven.CodeAnalysis;

/// <summary>
/// Indicates which reflection stack should load metadata references for a compilation.
/// </summary>
public enum MetadataReferenceHostKind
{
    /// <summary>
    /// Use the runtime <see cref="System.Reflection.MetadataLoadContext"/> APIs.
    /// </summary>
    Runtime,

    /// <summary>
    /// Use System.Reflection2's metadata-only load context.
    /// </summary>
    SystemReflection2,
}
