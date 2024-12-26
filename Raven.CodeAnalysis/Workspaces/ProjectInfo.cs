namespace Raven.CodeAnalysis;

public class ProjectInfo
{
    public ProjectInfo(
        ProjectAttributes attributes,
        CompilationOptions? compilationOptions,
        ParseOptions? parseOptions,
        IReadOnlyList<DocumentInfo> documents,
        IReadOnlyList<ProjectReference> projectReferences,
        IReadOnlyList<MetadataReference> metadataReferences,
        //IReadOnlyList<AnalyzerReference> analyzerReferences,
        IReadOnlyList<DocumentInfo> additionalDocuments //,
                                                        //IReadOnlyList<DocumentInfo> analyzerConfigDocuments
    )
    {
        Attributes = attributes;
        CompilationOptions = compilationOptions;
        ParseOptions = parseOptions;
        Documents = documents;
        ProjectReferences = projectReferences;
        MetadataReferences = metadataReferences;
        AdditionalDocuments = additionalDocuments;
    }

    internal ProjectAttributes Attributes { get; }

    public ProjectId Id => Attributes.Id;

    public VersionStamp Version => Attributes.Version;

    public string Name => Attributes.Name;

    public string AssemblyName => Attributes.AssemblyName;

    public string Language => Attributes.Language;

    public string? FilePath => Attributes.FilePath;

    public CompilationOptions? CompilationOptions { get; }

    public ParseOptions? ParseOptions { get; }

    public IReadOnlyList<DocumentInfo> Documents { get; }

    public IReadOnlyList<ProjectReference> ProjectReferences { get; }

    public IReadOnlyList<MetadataReference> MetadataReferences { get; }

    public IReadOnlyList<DocumentInfo> AdditionalDocuments { get; }

    internal ProjectInfo With(
        ProjectId id,
        VersionStamp version,
        string name,
        string assemblyName,
        string language,
        string? filePath = null
    )
    {
        return new ProjectInfo(
            Attributes.With(
                id,
                version,
                name,
                assemblyName,
                language,
                filePath
            ));
    }
}
