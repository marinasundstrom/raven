namespace Raven.CodeAnalysis;

public class DocumentAttributes
{
    public DocumentAttributes(
        DocumentId id,
        string name,
        IReadOnlyList<string> folders,
        SourceCodeKind sourceCodeKind,
        string? filePath,
        bool isGenerated,
        bool designTimeOnly)
    {
        Id = id;
        Name = name;
        Folders = folders;
        SourceCodeKind = sourceCodeKind;
        FilePath = filePath;
        IsGenerated = isGenerated;
        DesignTimeOnly = designTimeOnly;
    }

    public DocumentId Id { get; }
    public string Name { get; }
    public IReadOnlyList<string> Folders { get; }
    public SourceCodeKind SourceCodeKind { get; }
    public string? FilePath { get; }
    public bool IsGenerated { get; }
    public bool DesignTimeOnly { get; }

    public DocumentAttributes With(
        DocumentId id,
        string name,
        IReadOnlyList<string> folders,
        SourceCodeKind sourceCodeKind,
        string? filePath,
        bool isGenerated,
        bool designTimeOnly)
    {
        return new(id,
            name,
            folders,
            sourceCodeKind,
            filePath,
            isGenerated,
            designTimeOnly);
    }
}

public class ProjectAttributes
{
    internal ProjectAttributes(
        ProjectId id,
        VersionStamp version,
        string name,
        string assemblyName,
        string language,
        /*CompilationOutputInfo compilationOutputInfo,
        SourceHashAlgorithm checksumAlgorithm,
        string? defaultNamespace = null, */
        string? filePath = null
        /* string? outputFilePath = null,
        string? outputRefFilePath = null,
        Guid telemetryId = default,
        bool isSubmission = false,
        bool hasAllInformation = true,
        bool runAnalyzers = true,
        bool hasSdkCodeStyleAnalyzers = false */)
    {
        Id = id;
        Version = version;
        Name = name;
        AssemblyName = assemblyName;
        Language = language;
        FilePath = filePath;
    }

    public ProjectId Id { get; }
    public VersionStamp Version { get; }
    public string Name { get; }
    public string AssemblyName { get; }
    public string Language { get; }
    public string? FilePath { get; }

    public ProjectAttributes With(
           ProjectId id,
           VersionStamp version,
           string name,
           string assemblyName,
           string language,
           string? filePath = null
       )
    {
        return new(
            id,
            version,
            name,
            assemblyName,
            language,
            filePath);
    }
}

public class SolutionAttributes
{
    public SolutionAttributes(
        SolutionId id,
        VersionStamp version,
        string? filePath)
    {
        Id = id;
        Version = version;
        FilePath = filePath;
    }

    public SolutionId Id { get; internal set; }
    public VersionStamp Version { get; internal set; }
    public string? FilePath { get; internal set; }

    public SolutionAttributes With(
         SolutionId id,
         VersionStamp version,
         string? filePath = null
     )
    {
        return new(
            id,
            version,
            filePath);
    }
}