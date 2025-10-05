namespace System.Reflection2;

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Reflection.Metadata;

/// <summary>
/// Represents a lightweight reflection-only load context backed by <see cref="MetadataReader"/> instances.
/// </summary>
public sealed class MetadataLoadContext : IDisposable
{
    private readonly ConcurrentDictionary<string, MetadataAssembly> _assemblies = new(StringComparer.OrdinalIgnoreCase);
    private readonly List<IDisposable> _disposables = new();
    private readonly IMetadataAssemblyResolver _resolver;
    private bool _disposed;

    public MetadataLoadContext(IMetadataAssemblyResolver resolver)
    {
        _resolver = resolver ?? throw new ArgumentNullException(nameof(resolver));
    }

    /// <summary>
    /// Registers an assembly that was already materialized as a <see cref="MetadataReader"/>.
    /// </summary>
    /// <param name="reader">The metadata reader.</param>
    /// <param name="location">The optional location of the assembly.</param>
    /// <returns>The materialized <see cref="MetadataAssembly"/>.</returns>
    public MetadataAssembly RegisterAssembly(MetadataReader reader, string? location = null)
    {
        ThrowIfDisposed();
        var assembly = new MetadataAssembly(this, reader, location);
        _assemblies[GetCacheKey(assembly)] = assembly;
        return assembly;
    }

    /// <summary>
    /// Registers an assembly from a <see cref="MetadataReaderProvider"/> instance.
    /// </summary>
    /// <param name="provider">The provider that supplies the metadata reader.</param>
    /// <param name="location">The optional location of the assembly.</param>
    /// <returns>The materialized <see cref="MetadataAssembly"/>.</returns>
    public MetadataAssembly RegisterAssembly(MetadataReaderProvider provider, string? location = null)
        => RegisterAssembly(new MetadataResolutionResult(provider, location));

    /// <summary>
    /// Registers an assembly via a resolution result.
    /// </summary>
    /// <param name="result">The resolution result.</param>
    /// <returns>The materialized assembly.</returns>
    public MetadataAssembly RegisterAssembly(MetadataResolutionResult result)
    {
        ThrowIfDisposed();
        var reader = result.Provider.GetMetadataReader();
        var assembly = new MetadataAssembly(this, reader, result.Location);
        _disposables.Add(result);
        _assemblies[GetCacheKey(assembly)] = assembly;
        return assembly;
    }

    internal MetadataAssembly? Resolve(string name)
    {
        if (_assemblies.TryGetValue(name, out var assembly))
        {
            return assembly;
        }

        if (_resolver.TryResolve(name, out var result) && result is not null)
        {
            return RegisterAssembly(result);
        }

        return null;
    }

    internal MetadataAssembly? Resolve(MetadataReader reader, AssemblyReferenceHandle handle)
    {
        var reference = reader.GetAssemblyReference(handle);
        var name = reader.GetString(reference.Name);
        if (_assemblies.TryGetValue(name, out var assembly))
        {
            return assembly;
        }

        if (_resolver.TryResolve(reader, handle, out var result) && result is not null)
        {
            return RegisterAssembly(result);
        }

        return null;
    }

    private static string GetCacheKey(MetadataAssembly assembly)
        => assembly.GetName().Name ?? assembly.FullName ?? Guid.NewGuid().ToString();

    private void ThrowIfDisposed()
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
    }

    public void Dispose()
    {
        if (_disposed)
        {
            return;
        }

        foreach (var disposable in _disposables)
        {
            disposable.Dispose();
        }

        _disposables.Clear();
        _assemblies.Clear();
        _disposed = true;
    }
}
