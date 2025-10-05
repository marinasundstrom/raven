namespace System.Reflection2;

using System;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

/// <summary>
/// Represents the outcome of resolving an assembly to metadata.
/// </summary>
public sealed class MetadataResolutionResult : IDisposable
{
    public MetadataResolutionResult(MetadataReaderProvider provider, string? location = null, PEReader? peReader = null)
    {
        Provider = provider ?? throw new ArgumentNullException(nameof(provider));
        Location = location;
        PeReader = peReader;
        MethodBodyProvider = peReader is null ? null : peReader.GetMethodBody;
    }

    /// <summary>
    /// Gets the metadata provider.
    /// </summary>
    public MetadataReaderProvider Provider { get; }

    /// <summary>
    /// Gets an optional location associated with the assembly.
    /// </summary>
    public string? Location { get; }

    internal PEReader? PeReader { get; }

    internal Func<int, MethodBodyBlock?>? MethodBodyProvider { get; }

    public void Dispose()
    {
        Provider.Dispose();
        PeReader?.Dispose();
    }
}
