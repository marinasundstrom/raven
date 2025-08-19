using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>Immutable metadata about a project.</summary>
public sealed class ProjectInfo
{
    public ProjectInfo(
        ProjectAttributes attributes,
        IEnumerable<DocumentInfo> documents,
        IEnumerable<ProjectReference>? projectReferences = null,
        IEnumerable<MetadataReference>? metadataReferences = null)
    {
        Attributes = attributes;
        Documents = documents.ToImmutableArray();
        ProjectReferences = projectReferences?.ToImmutableArray() ?? ImmutableArray<ProjectReference>.Empty;
        MetadataReferences = metadataReferences?.ToImmutableArray() ?? ImmutableArray<MetadataReference>.Empty;
    }

    public ProjectAttributes Attributes { get; }
    public ProjectId Id => Attributes.Id;
    public string Name => Attributes.Name;
    public VersionStamp Version => Attributes.Version;

    public ImmutableArray<DocumentInfo> Documents { get; }

    public ImmutableArray<ProjectReference> ProjectReferences { get; }
    public ImmutableArray<MetadataReference> MetadataReferences { get; }
    public string? OutputFilePath { get; internal set; }
    public ParseOptions? ParseOptions { get; internal set; }
    public string? DefaultNamespace { get; internal set; }

    public ProjectInfo WithDocuments(IEnumerable<DocumentInfo> docs) =>
        new(Attributes, docs, ProjectReferences, MetadataReferences);

    public ProjectInfo WithVersion(VersionStamp version)
    {
        var newAttributes = Attributes with
        {
            Version = version
        };
        return new ProjectInfo(newAttributes, Documents, ProjectReferences, MetadataReferences);
    }

    public ProjectInfo WithProjectReferences(IEnumerable<ProjectReference> projectReferences) =>
        new(Attributes, Documents, projectReferences, MetadataReferences);

    public ProjectInfo WithMetadataReferences(IEnumerable<MetadataReference> metadataReferences) =>
        new(Attributes, Documents, ProjectReferences, metadataReferences);

    public sealed record ProjectAttributes(ProjectId Id, string Name, VersionStamp Version);
}
