using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

namespace Raven.CodeAnalysis.Metadata;

// Constructs a .NET context over an explicit reference set. Host registration
// and session reuse remain outside this component.
internal static class DotNetMetadataContextFactory
{
    internal static MetadataLoadContext Create(
        DotNetMetadataReferenceSet references,
        string? coreAssemblyName)
    {
        var resolver = new StreamBackedPathAssemblyResolver(references);
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

        public StreamBackedPathAssemblyResolver(DotNetMetadataReferenceSet references)
        {
            _pathsByIdentity = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            _pathsBySimpleName = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

            foreach (var reference in references.References)
            {
                if (!string.IsNullOrWhiteSpace(reference.FullName))
                    _pathsByIdentity.TryAdd(reference.FullName, reference.Path);

                if (!string.IsNullOrWhiteSpace(reference.SimpleName))
                    _pathsBySimpleName.TryAdd(reference.SimpleName, reference.Path);
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
