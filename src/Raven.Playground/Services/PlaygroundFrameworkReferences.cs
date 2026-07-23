using System.Reflection;

using Raven.CodeAnalysis;

namespace Raven.Playground.Services;

public sealed class PlaygroundFrameworkReferences
{
    private const string ResourcePrefix = "Raven.Playground.Framework.";

    private readonly Lazy<MetadataReference[]> _references = new(CreateReferences);

    public MetadataReference[] GetReferences() => _references.Value;

    private static MetadataReference[] CreateReferences()
    {
        var resourceAssembly = typeof(PlaygroundFrameworkReferences).Assembly;
        var referenceDirectory = Path.Combine(Path.GetTempPath(), "raven-playground-framework");
        Directory.CreateDirectory(referenceDirectory);

        return resourceAssembly
            .GetManifestResourceNames()
            .Where(resourceName => resourceName.StartsWith(ResourcePrefix, StringComparison.Ordinal)
                && resourceName.EndsWith(".dll", StringComparison.Ordinal))
            .Order(StringComparer.Ordinal)
            .Select(resourceName =>
            {
                using var resource = resourceAssembly.GetManifestResourceStream(resourceName)
                    ?? throw new InvalidOperationException($"Missing embedded framework reference '{resourceName}'.");
                using var image = new MemoryStream();
                resource.CopyTo(image);

                var assemblyName = resourceName[ResourcePrefix.Length..^".dll".Length];
                return (MetadataReference)MetadataReference.CreateFromImage(
                    image.ToArray(),
                    Path.Combine(referenceDirectory, $"{assemblyName}.dll"));
            })
            .ToArray();
    }
}
