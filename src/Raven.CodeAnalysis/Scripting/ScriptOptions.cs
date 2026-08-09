using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Scripting;

/// <summary>
/// Configures compilation of Raven script submissions.
/// </summary>
public sealed class ScriptOptions
{
    private static readonly ScriptOptions s_default = new(
        RuntimeMetadataReferenceResolver.CreateReferences());

    private ScriptOptions(ImmutableArray<MetadataReference> references)
    {
        MetadataReferences = references;
    }

    /// <summary>
    /// Gets the default script options, including platform and loaded assembly references.
    /// </summary>
    public static ScriptOptions Default => s_default;

    /// <summary>
    /// Gets the metadata references available to each submission.
    /// </summary>
    public ImmutableArray<MetadataReference> MetadataReferences { get; }

    /// <summary>
    /// Returns options with the specified metadata references appended.
    /// </summary>
    public ScriptOptions AddReferences(params MetadataReference[] references)
    {
        ArgumentNullException.ThrowIfNull(references);
        if (references.Any(static reference => reference is null))
            throw new ArgumentException("Metadata references cannot contain null values.", nameof(references));

        return new ScriptOptions(MetadataReferences.AddRange(references));
    }
}
