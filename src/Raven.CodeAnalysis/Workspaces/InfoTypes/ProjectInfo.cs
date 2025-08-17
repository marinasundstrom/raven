using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>Immutable metadata about a project.</summary>
public sealed class ProjectInfo
{
    public ProjectInfo(ProjectAttributes attributes,
                       IEnumerable<DocumentInfo> documents)
    {
        Attributes = attributes;
        Documents = documents.ToImmutableArray();
    }

    public ProjectAttributes Attributes { get; }
    public ProjectId Id => Attributes.Id;
    public string Name => Attributes.Name;
    public VersionStamp Version => Attributes.Version;

    public ImmutableArray<DocumentInfo> Documents { get; }

    public IReadOnlyList<ProjectReference> ProjectReferences { get; } = [];
    public IReadOnlyList<MetadataReference> MetadataReferences { get; } = [];
    public string? OutputFilePath { get; internal set; }
    public ParseOptions? ParseOptions { get; internal set; }
    public string? DefaultNamespace { get; internal set; }

    public ProjectInfo WithDocuments(IEnumerable<DocumentInfo> docs) =>
        new(Attributes, docs);

    public ProjectInfo WithVersion(VersionStamp version)
    {
        var newAttributes = Attributes with
        {
            Version = version
        };
        return new ProjectInfo(newAttributes, Documents);
    }

    public sealed record ProjectAttributes(ProjectId Id, string Name, VersionStamp Version);
}
