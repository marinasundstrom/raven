using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

/// <summary>Immutable metadata about a project.</summary>
public sealed class ProjectInfo
{
    public ProjectInfo(
        ProjectAttributes attributes,
        IEnumerable<DocumentInfo> documents,
        IEnumerable<ProjectReference>? projectReferences = null,
        IEnumerable<MetadataReference>? metadataReferences = null,
        IEnumerable<AnalyzerReference>? analyzerReferences = null,
        string? filePath = null,
        string? targetFramework = null,
        CompilationOptions? compilationOptions = null,
        string? assemblyName = null)
    {
        Attributes = attributes;
        Documents = documents.ToImmutableArray();
        ProjectReferences = projectReferences?.ToImmutableArray() ?? ImmutableArray<ProjectReference>.Empty;
        MetadataReferences = metadataReferences?.ToImmutableArray() ?? ImmutableArray<MetadataReference>.Empty;
        AnalyzerReferences = analyzerReferences?.ToImmutableArray() ?? ImmutableArray<AnalyzerReference>.Empty;
        FilePath = filePath;
        TargetFramework = targetFramework;
        CompilationOptions = compilationOptions;
        AssemblyName = assemblyName;
        DefaultNamespace = compilationOptions?.RootNamespace;
    }

    public ProjectAttributes Attributes { get; }
    public ProjectId Id => Attributes.Id;
    public string Name => Attributes.Name;
    public VersionStamp Version => Attributes.Version;

    public ImmutableArray<DocumentInfo> Documents { get; }

    public ImmutableArray<ProjectReference> ProjectReferences { get; }
    public ImmutableArray<MetadataReference> MetadataReferences { get; }
    public ImmutableArray<AnalyzerReference> AnalyzerReferences { get; }
    public CompilationOptions? CompilationOptions { get; }
    public string? AssemblyName { get; }
    public ParseOptions? ParseOptions { get; internal set; }
    public string? DefaultNamespace { get; internal set; }
    public string? FilePath { get; }
    public string? TargetFramework { get; }

    public ProjectInfo WithDocuments(IEnumerable<DocumentInfo> docs) =>
        new(Attributes, docs, ProjectReferences, MetadataReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName);

    public ProjectInfo WithVersion(VersionStamp version)
    {
        var newAttributes = Attributes with
        {
            Version = version
        };
        return new ProjectInfo(newAttributes, Documents, ProjectReferences, MetadataReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName);
    }

    public ProjectInfo WithProjectReferences(IEnumerable<ProjectReference> projectReferences) =>
        new(Attributes, Documents, projectReferences, MetadataReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName);

    public ProjectInfo WithMetadataReferences(IEnumerable<MetadataReference> metadataReferences) =>
        new(Attributes, Documents, ProjectReferences, metadataReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName);

    public ProjectInfo WithCompilationOptions(CompilationOptions? compilationOptions) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, AnalyzerReferences, FilePath, TargetFramework, compilationOptions, AssemblyName);

    public ProjectInfo WithAnalyzerReferences(IEnumerable<AnalyzerReference> analyzerReferences) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, analyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName);

    public sealed record ProjectAttributes(ProjectId Id, string Name, VersionStamp Version);
}
