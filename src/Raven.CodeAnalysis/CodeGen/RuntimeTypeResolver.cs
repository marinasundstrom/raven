using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.CodeGen;

/// <summary>
/// Provides runtime <see cref="Type"/> handles for the code generator by inspecting
/// metadata references with <see cref="System.Reflection.Metadata"/> and mapping them
/// to runtime assemblies.
/// </summary>
internal sealed class RuntimeTypeResolver
{
    private readonly ImmutableArray<Assembly> _assemblies;
    private readonly ConcurrentDictionary<string, Type> _typeCache = new(StringComparer.Ordinal);

    public RuntimeTypeResolver(Compilation compilation)
    {
        if (compilation is null)
            throw new ArgumentNullException(nameof(compilation));

        compilation.EnsureSetup();

        CoreAssembly = typeof(object).Assembly;

        var assemblies = ImmutableArray.CreateBuilder<Assembly>();
        assemblies.Add(CoreAssembly);

        var catalog = compilation.ReferenceCatalog;

        foreach (var reference in compilation.References)
        {
            if (reference is not PortableExecutableReference peReference)
                continue;

            var runtimeAssembly = catalog.TryGetImplementationAssembly(peReference);
            if (runtimeAssembly is null)
                continue;

            if (!assemblies.Contains(runtimeAssembly))
                assemblies.Add(runtimeAssembly);
        }

        _assemblies = assemblies.ToImmutable();
    }

    public Assembly CoreAssembly { get; }

    public Type GetRequiredType(string metadataName)
    {
        if (metadataName is null)
            throw new ArgumentNullException(nameof(metadataName));

        if (_typeCache.TryGetValue(metadataName, out var cached))
            return cached;

        foreach (var assembly in _assemblies)
        {
            var resolved = assembly.GetType(metadataName, throwOnError: false, ignoreCase: false);
            if (resolved is not null)
            {
                _typeCache.TryAdd(metadataName, resolved);
                return resolved;
            }
        }

        foreach (var assembly in _assemblies)
        {
            var qualifiedName = string.Concat(metadataName, ", ", assembly.FullName);
            var resolved = Type.GetType(qualifiedName, throwOnError: false, ignoreCase: false);
            if (resolved is not null)
            {
                _typeCache.TryAdd(metadataName, resolved);
                return resolved;
            }
        }

        throw new InvalidOperationException($"Unable to resolve runtime type '{metadataName}'.");
    }

}
