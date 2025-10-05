namespace System.Reflection2;

using System;
using System.Reflection.Metadata;

/// <summary>
/// Represents the outcome of resolving an assembly to metadata.
/// </summary>
public sealed class MetadataResolutionResult : IDisposable
{
    public MetadataResolutionResult(MetadataReaderProvider provider, string? location = null)
    {
        Provider = provider ?? throw new ArgumentNullException(nameof(provider));
        Location = location;
    }

    /// <summary>
    /// Gets the metadata provider.
    /// </summary>
    public MetadataReaderProvider Provider { get; }

    /// <summary>
    /// Gets an optional location associated with the assembly.
    /// </summary>
    public string? Location { get; }

    public void Dispose()
    {
        Provider.Dispose();
    }
}
