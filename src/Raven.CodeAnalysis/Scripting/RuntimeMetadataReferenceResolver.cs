using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Scripting;

internal static class RuntimeMetadataReferenceResolver
{
    internal static ImmutableArray<MetadataReference> CreateReferences(
        IEnumerable<MetadataReference>? additionalReferences = null)
    {
        var references = new Dictionary<string, MetadataReference>(StringComparer.OrdinalIgnoreCase);

        void AddPath(string? path)
        {
            if (string.IsNullOrWhiteSpace(path) || !File.Exists(path))
                return;

            var fullPath = Path.GetFullPath(path);
            references.TryAdd(fullPath, MetadataReference.CreateFromFile(fullPath));
        }

        if (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") is string platformAssemblies)
        {
            foreach (var path in platformAssemblies.Split(
                         Path.PathSeparator,
                         StringSplitOptions.RemoveEmptyEntries))
            {
                AddPath(path);
            }
        }

        foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
        {
            if (!assembly.IsDynamic)
                AddPath(assembly.Location);
        }

        if (additionalReferences is not null)
        {
            foreach (var reference in additionalReferences.OfType<PortableExecutableReference>())
                AddPath(reference.FilePath);
        }

        return references.Values.ToImmutableArray();
    }
}
