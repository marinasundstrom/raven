using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

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

        CoreAssembly = typeof(object).Assembly;

        var assemblies = ImmutableArray.CreateBuilder<Assembly>();
        assemblies.Add(CoreAssembly);

        foreach (var assembly in LoadReferencedAssemblies(compilation.References))
        {
            if (!assemblies.Contains(assembly))
                assemblies.Add(assembly);
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

    private static IEnumerable<Assembly> LoadReferencedAssemblies(IEnumerable<MetadataReference> references)
    {
        foreach (var reference in references)
        {
            if (reference is not PortableExecutableReference peReference)
                continue;

            if (!File.Exists(peReference.FilePath))
                continue;

            AssemblyName? assemblyIdentity = null;

            using (var stream = File.OpenRead(peReference.FilePath))
            using (var peReader = new PEReader(stream))
            {
                var reader = peReader.GetMetadataReader();
                if (!reader.IsAssembly)
                    continue;

                var definition = reader.GetAssemblyDefinition();
                var name = reader.GetString(definition.Name);
                var culture = reader.GetString(definition.Culture);

                assemblyIdentity = new AssemblyName(name)
                {
                    Version = definition.Version,
                    CultureName = string.IsNullOrEmpty(culture) ? null : culture
                };

                if (!definition.PublicKey.IsNil)
                    assemblyIdentity.SetPublicKey(reader.GetBlobBytes(definition.PublicKey));

                if ((definition.Flags & AssemblyFlags.PublicKey) != 0)
                    assemblyIdentity.Flags |= AssemblyNameFlags.PublicKey;
            }

            if (assemblyIdentity is null)
                continue;

            Assembly? runtimeAssembly = null;

            try
            {
                runtimeAssembly = Assembly.Load(assemblyIdentity);
            }
            catch
            {
                // The reference might point to a reference assembly. In that case, fall back to
                // probing by file path which works for implementation assemblies that ship next to
                // the reference.
                try
                {
                    runtimeAssembly = Assembly.LoadFrom(peReference.FilePath);
                }
                catch
                {
                    // ignored – we simply skip assemblies we cannot materialize at runtime.
                }
            }

            if (runtimeAssembly is not null)
                yield return runtimeAssembly;
        }
    }
}
