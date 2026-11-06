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

            if (TryGetFullPath(path) is not string fullPath || !File.Exists(fullPath))
                continue;
            if (TryReadAssemblyName(fullPath) is not { } identity || string.IsNullOrWhiteSpace(identity.FullName))
                continue;

            // Prefer the first reference surface supplied for each identity.
            pathByAssemblyIdentity.TryAdd(identity.FullName, fullPath);
        }

        var normalizedPaths = pathByAssemblyIdentity.Values
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        var references = ImmutableArray.CreateBuilder<Reference>();
        foreach (var path in normalizedPaths)
        {
            // A candidate may have disappeared since path normalization.
            if (TryReadAssemblyName(path) is not { } identity)
                continue;

            references.Add(new Reference(identity.FullName, identity.Name, path));
        }

        return new DotNetMetadataReferenceSet(references.ToImmutable());
    }

    private static string? TryGetFullPath(string path)
    {
        try
        {
            return Path.GetFullPath(path);
        }
        catch
        {
            return null;
        }
    }

    private static System.Reflection.AssemblyName? TryReadAssemblyName(string path)
    {
        try
        {
            return DotNetMetadataContextFactory.ReadAssemblyName(path);
        }
        catch
        {
            return null;
        }
    }

    internal readonly record struct Reference(string? FullName, string? SimpleName, string Path);
}
