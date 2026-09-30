using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis.Metadata;

// An immutable snapshot of the .NET resolver inputs. It contains no host
// registration callbacks, compilation state, or mutable reflection identities.
internal sealed class DotNetMetadataReferenceSet
{
    private DotNetMetadataReferenceSet(ImmutableArray<Reference> references)
    {
        References = references;
    }

    internal ImmutableArray<Reference> References { get; }

    internal static DotNetMetadataReferenceSet Create(IEnumerable<string> paths)
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
                assemblyIdentity = DotNetMetadataContextFactory.ReadAssemblyName(fullPath);
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

        var references = ImmutableArray.CreateBuilder<Reference>();
        foreach (var path in normalizedPaths)
        {
            System.Reflection.AssemblyName identity;
            try
            {
                identity = DotNetMetadataContextFactory.ReadAssemblyName(path);
            }
            catch
            {
                // Preserve the resolver's second admission check: a candidate
                // may have disappeared or changed since path normalization.
                continue;
            }

            references.Add(new Reference(identity.FullName, identity.Name, path));
        }

        return new DotNetMetadataReferenceSet(references.ToImmutable());
    }

    internal readonly record struct Reference(string? FullName, string? SimpleName, string Path);
}
