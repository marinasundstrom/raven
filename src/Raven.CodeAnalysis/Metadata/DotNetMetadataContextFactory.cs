using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

namespace Raven.CodeAnalysis.Metadata;

// Owns .NET metadata context construction. Reference policy and context reuse
// remain with Compilation until the metadata-session boundary is extracted.
internal static class DotNetMetadataContextFactory
{
    internal static MetadataLoadContext Create(
        IEnumerable<string> paths,
        string? coreAssemblyName,
        Action<string, string> registerAssemblyPath)
    {
        var pathByAssemblyIdentity = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

        foreach (var path in paths)
        {
            if (string.IsNullOrWhiteSpace(path))
                continue;

            string fullPath;
            try
            {
                fullPath = Path.GetFullPath(path);
            }
            catch
            {
                continue;
            }

            if (!File.Exists(fullPath))
                continue;

            System.Reflection.AssemblyName assemblyIdentity;
            try
            {
                assemblyIdentity = ReadAssemblyName(fullPath);
            }
            catch
            {
                continue;
            }

            if (string.IsNullOrWhiteSpace(assemblyIdentity.FullName))
                continue;

            var identityKey = assemblyIdentity.FullName;

            if (!pathByAssemblyIdentity.TryGetValue(identityKey, out var existingPath))
            {
                pathByAssemblyIdentity[identityKey] = fullPath;
                continue;
            }

            // Keep the first path for an identity (typically reference assemblies from project metadata).
            // Preferring runtime assemblies here can hide reference-surface namespaces during binding.
        }

        var normalizedPaths = pathByAssemblyIdentity.Values
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        var resolver = new StreamBackedPathAssemblyResolver(normalizedPaths, registerAssemblyPath);
        var resolvedCoreAssemblyName = string.IsNullOrWhiteSpace(coreAssemblyName) ? "System.Private.CoreLib" : coreAssemblyName;
        return new MetadataLoadContext(resolver, resolvedCoreAssemblyName);
    }

    internal static System.Reflection.AssemblyName ReadAssemblyName(string path)
    {
        if (!OperatingSystem.IsBrowser() && !OperatingSystem.IsWasi())
            return System.Reflection.AssemblyName.GetAssemblyName(path);

        return ReadAssemblyNameFromMetadata(path);
    }

    internal static System.Reflection.AssemblyName ReadAssemblyNameFromMetadata(string path)
    {
        using var stream = File.OpenRead(path);
        using var peReader = new PEReader(stream);
        if (!peReader.HasMetadata)
            throw new BadImageFormatException($"'{path}' does not contain managed metadata.");

        var metadataReader = peReader.GetMetadataReader();
        if (!metadataReader.IsAssembly)
            throw new BadImageFormatException($"'{path}' is not a managed assembly.");

        var definition = metadataReader.GetAssemblyDefinition();
        var assemblyName = new System.Reflection.AssemblyName
        {
            Name = metadataReader.GetString(definition.Name),
            Version = definition.Version,
            CultureName = definition.Culture.IsNil ? null : metadataReader.GetString(definition.Culture),
            Flags = (AssemblyNameFlags)definition.Flags,
        };

        if (!definition.PublicKey.IsNil)
            assemblyName.SetPublicKey(metadataReader.GetBlobBytes(definition.PublicKey));

        return assemblyName;
    }

    private sealed class StreamBackedPathAssemblyResolver : MetadataAssemblyResolver
    {
        private readonly Dictionary<string, string> _pathsByIdentity;
        private readonly Dictionary<string, string> _pathsBySimpleName;

        public StreamBackedPathAssemblyResolver(IEnumerable<string> paths, Action<string, string> registerAssemblyPath)
        {
            _pathsByIdentity = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            _pathsBySimpleName = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

            foreach (var path in paths)
            {
                System.Reflection.AssemblyName identity;
                try
                {
                    identity = ReadAssemblyName(path);
                }
                catch
                {
                    continue;
                }

                if (!string.IsNullOrWhiteSpace(identity.FullName))
                    _pathsByIdentity.TryAdd(identity.FullName, path);

                if (!string.IsNullOrWhiteSpace(identity.Name))
                {
                    _pathsBySimpleName.TryAdd(identity.Name, path);
                    registerAssemblyPath(identity.Name, path);
                }
            }
        }

        public override Assembly? Resolve(MetadataLoadContext context, System.Reflection.AssemblyName assemblyName)
        {
            if (!string.IsNullOrWhiteSpace(assemblyName.FullName) &&
                _pathsByIdentity.TryGetValue(assemblyName.FullName, out var path))
            {
                return LoadFromPath(context, path);
            }

            if (!string.IsNullOrWhiteSpace(assemblyName.Name) &&
                _pathsBySimpleName.TryGetValue(assemblyName.Name, out path))
            {
                return LoadFromPath(context, path);
            }

            return null;
        }

        private static Assembly LoadFromPath(MetadataLoadContext context, string path)
        {
            var bytes = File.ReadAllBytes(path);
            return context.LoadFromStream(new MemoryStream(bytes));
        }
    }
}
