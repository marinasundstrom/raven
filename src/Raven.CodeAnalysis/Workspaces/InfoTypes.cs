using System.Net.Sockets;

namespace Raven.CodeAnalysis;

public class DocumentInfo
{
    internal DocumentAttributes Attributes { get; }

    public DocumentId Id => Attributes.Id;

    public string Name => Attributes.Name;

    public SourceCodeKind SourceCodeKind => Attributes.SourceCodeKind;

    public string? FilePath => Attributes.FilePath;

    public bool IsGenerated => Attributes.IsGenerated;

    public bool DesignTimeOnly => Attributes.DesignTimeOnly;

    public DocumentInfo(DocumentAttributes attributes)
    {
        Attributes = attributes;
    }

    public static DocumentInfo Create(
        DocumentId id,
        string name,
        IReadOnlyList<string> folders,
        SourceCodeKind sourceCodeKind,
        string? filePath,
        bool isGenerated,
        bool designTimeOnly) =>
    new(
        new DocumentAttributes(
            id,
            name,
            folders,
            sourceCodeKind,
            filePath,
            isGenerated,
            designTimeOnly));

    internal DocumentInfo With(
        DocumentId id,
        string name,
        IReadOnlyList<string> folders,
        SourceCodeKind sourceCodeKind,
        string? filePath,
        bool isGenerated,
        bool designTimeOnly
    )
    {
        return new(Attributes.With(
            id,
            name,
            folders,
            sourceCodeKind,
            filePath,
            isGenerated,
            designTimeOnly
        ));
    }
}

public class ProjectInfo
{
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

    private ProjectInfo(
        ProjectAttributes attributes,
        CompilationOptions? compilationOptions,
        ParseOptions? parseOptions,
        IReadOnlyList<DocumentInfo> documents,
        IReadOnlyList<ProjectReference> projectReferences,
        IReadOnlyList<MetadataReference> metadataReferences,
        //IReadOnlyList<AnalyzerReference> analyzerReferences,
        IReadOnlyList<DocumentInfo> additionalDocuments /*,
        //IReadOnlyList<DocumentInfo> analyzerConfigDocuments */
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


    public static ProjectInfo Create(
        ProjectId id,
        VersionStamp version,
        string name,
        string assemblyName,
        string language)
    {
        return Create(id, version, name, assemblyName, language, null, null, null, [], [], [], []);
    }

    public static ProjectInfo Create(
        ProjectId id,
        VersionStamp version,
        string name,
        string assemblyName,
        string language,
        /*CompilationOutputInfo compilationOutputInfo,
        SourceHashAlgorithm checksumAlgorithm,
        string? defaultNamespace = null, */
        string? filePath,
        /* string? outputFilePath = null,
        string? outputRefFilePath = null,
        Guid telemetryId = default,
        bool isSubmission = false,
        bool hasAllInformation = true,
        bool runAnalyzers = true,
        bool hasSdkCodeStyleAnalyzers = false */
        CompilationOptions? compilationOptions,
        ParseOptions? parseOptions,
        IReadOnlyList<DocumentInfo> documents,
        IReadOnlyList<ProjectReference> projectReferences,
        IReadOnlyList<MetadataReference> metadataReferences,
        //IReadOnlyList<AnalyzerReference> analyzerReferences,
        IReadOnlyList<DocumentInfo> additionalDocuments /*,
        //IReadOnlyList<DocumentInfo> analyzerConfigDocuments */
    ) => new(
            new ProjectAttributes(
                id,
                version,
                name,
                assemblyName,
                language,
                filePath
            ),
            compilationOptions,
            parseOptions,
            documents,
            projectReferences,
            metadataReferences,
            additionalDocuments);

    internal ProjectInfo With(
        ProjectId id,
        VersionStamp version,
        string name,
        string assemblyName,
        string language,
        string? filePath = null
    ) => new(
                Attributes.With(
                    id,
                    version,
                    name,
                    assemblyName,
                    language,
                    filePath
                ),
                null,
                null,
                Documents.ToList(),
                ProjectReferences.ToList(),
                MetadataReferences.ToList(),
                AdditionalDocuments.ToList()
            );
}

public class SolutionInfo
{
    internal SolutionAttributes Attributes { get; }

    public SolutionId Id => Attributes.Id;

    public VersionStamp Version => Attributes.Version;

    public string? FilePath => Attributes.FilePath;

    public IReadOnlyList<ProjectInfo> Projects { get; }

    private SolutionInfo(
        SolutionAttributes attributes,
        IReadOnlyList<ProjectInfo> projects)
    {
        Attributes = attributes;
        Projects = projects;
    }

    public static SolutionInfo Create(
        SolutionId id,
        VersionStamp version,
        string? filePath,
        IReadOnlyList<ProjectInfo> projects) =>
        new SolutionInfo(
            new SolutionAttributes(id, version, filePath),
            projects.ToList());
}