using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;

namespace Raven.Playground.Services;

public sealed class PlaygroundFrameworkReferences
{
    private const string ResourcePrefix = "Raven.Playground.Framework.";

    private readonly Lazy<ReferenceSet> _references = new(CreateReferences);

    public MetadataReference[] GetReferences() => _references.Value.MetadataReferences;

    public MacroReference[] GetMacroReferences() => _references.Value.MacroReferences;

    private static ReferenceSet CreateReferences()
    {
        var resourceAssembly = typeof(PlaygroundFrameworkReferences).Assembly;
        var referenceDirectory = Path.Combine(Path.GetTempPath(), "raven-playground-framework");
        Directory.CreateDirectory(referenceDirectory);
        var metadataReferences = new List<MetadataReference>();
        var macroReferences = new List<MacroReference>();

        foreach (var resourceName in resourceAssembly
            .GetManifestResourceNames()
            .Where(resourceName => resourceName.StartsWith(ResourcePrefix, StringComparison.Ordinal)
                && resourceName.EndsWith(".dll", StringComparison.Ordinal))
            .Order(StringComparer.Ordinal))
        {
            using var resource = resourceAssembly.GetManifestResourceStream(resourceName)
                ?? throw new InvalidOperationException($"Missing embedded framework reference '{resourceName}'.");
            using var image = new MemoryStream();
            resource.CopyTo(image);

            var assemblyName = resourceName[ResourcePrefix.Length..^".dll".Length];
            var assemblyImage = image.ToArray();
            metadataReferences.Add(MetadataReference.CreateFromImage(
                assemblyImage,
                Path.Combine(referenceDirectory, $"{assemblyName}.dll")));
            if (string.Equals(assemblyName, "Raven.Macros", StringComparison.Ordinal))
            {
                var referencePath = Path.Combine(
                    referenceDirectory,
                    $"{assemblyName}.dll");
                var macroAssembly = AppDomain.CurrentDomain.GetAssemblies()
                    .FirstOrDefault(static assembly =>
                        string.Equals(
                            assembly.GetName().Name,
                            "Raven.Macros",
                            StringComparison.Ordinal))
                    ?? Assembly.Load(new AssemblyName("Raven.Macros"));
                macroReferences.Add(MacroReference.CreateFromAssembly(
                    macroAssembly,
                    referencePath));
            }
        }

        return new ReferenceSet(
            metadataReferences.ToArray(),
            macroReferences.ToArray());
    }

    private sealed record ReferenceSet(
        MetadataReference[] MetadataReferences,
        MacroReference[] MacroReferences);
}
